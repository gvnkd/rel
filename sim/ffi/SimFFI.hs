module SimFFI where

import Control.Exception (SomeException, catch, fromException, ioError, try)
import Control.Monad (forM, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as VU
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Sim.Cli (applyArgs)
import qualified Sim.DtwIndex as DTW
import Sim.Csv (parseMatchesHdr, relsBin)
import Sim.Dtw (Prefix, dtwPathFree, mkPrefix, pfLen, stepCost, zslice, zwin)
import qualified Sim.DtwSelect as SEL
import Sim.Entropy (rollingCECP, rollingPermEntropy)
import Sim.Fft (fftApprox, fftBest)
import Sim.Gen (GenState (..))
import Sim.Ind (bollingerSeries, macdSeries, rsiSeries, smaSeries)
import Sim.Markov (MarkovParams (..), markovSeriesStates)
import qualified Sim.ML.Data as MLD
import qualified Sim.ML.Train as MLT
import qualified Sim.Run as Run
import Sim.Resume (readState, statePath, writeState)
import qualified Sim.Strat.Dsl as STD
import qualified Sim.Strat.Optimize as STO
import Sim.Types (SimHooks (..), SimMode (..), SimParams (..), SimCancelled (..))
import System.Directory (doesFileExist)
import System.IO.Error (isDoesNotExistError, userError)

-- params are "key=value" pairs separated by ';', keys as in the CLI
-- without the leading "--" (e.g. "candles=1000;seed=7")
--
-- returns 0 on success, 1 on bad params, 2 on an IO/generation
-- failure, 3 when cancelled via sim_cancel (partial output removed)
foreign export ccall "sim_generate" simGenerate :: CString -> CString -> IO CInt

-- continue a previously generated timeline: reads the state snapshot
-- next to out_path (<basename>.state), generates spCandles MORE candles
-- with the snapshot's parameters (only the candle count comes from the
-- given params), and appends them to the candles csv and the ticks
-- sidecar (sidecar rows continue the candle numbering). A fresh
-- <basename>.state is written on success.
--
-- returns 0 on success, 1 on bad params, 2 on an IO/generation
-- failure, 3 when cancelled via sim_cancel (the partially appended
-- sidecar is truncated back, the candles csv is untouched), 4 when
-- there is no state to continue from
foreign export ccall "sim_continue" simContinue :: CString -> CString -> IO CInt

-- generation progress as a 0..1 fraction, polled from the UI thread
-- while a sim_generate call is running on a worker thread
foreign export ccall "sim_progress" simProgress :: IO CDouble

-- request cancellation of the in-flight sim_generate (idempotent)
foreign export ccall "sim_cancel" simCancel :: IO ()

-- FFT fit + forecast over closes[start..end): writes (window length +
-- predict) doubles — the ridge-LS fit over the window followed by the
-- forecast — into out, returns the total count (0 on invalid input);
-- edgeW > 1 endpoint-weights the fit
foreign export ccall "sim_fft_forecast" simFftForecast
  :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> Ptr CDouble -> IO CInt

simFftForecast :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> CDouble -> Ptr CDouble -> IO CInt
simFftForecast ptr (CInt count) (CInt start) (CInt end) (CInt predict) (CInt waves) (CDouble edgeW) out
  | count <= 0 || predict <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let (fit, fc) = fftApprox (fromIntegral waves) edgeW (map (\(CDouble d) -> d) closes) (fromIntegral start) (fromIntegral end) (fromIntegral predict)
          allOut = fit ++ fc
      if length fc /= fromIntegral predict
        then pure 0
        else do
          pokeArray out (map CDouble allOut)
          pure (CInt (fromIntegral (length allOut)))

-- walk-forward search for the best (window, waves) pair for `predict`:
-- writes [bestWindow, bestWaves, bestScore] into out, returns 1 on
-- success, 0 when no candidate has a valid anchor
foreign export ccall "sim_fft_best" simFftBest
  :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt

simFftBest :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt
simFftBest ptr (CInt count) (CInt predict) (CInt pct) out
  | count <= 0 || predict <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let (win, waves, sc) = fftBest (fromIntegral pct) (fromIntegral predict) (map (\(CDouble d) -> d) closes)
      if win <= 0
        then pure 0
        else do
          pokeArray out [CDouble (fromIntegral win), CDouble (fromIntegral waves), CDouble sc]
          pure 1

-- rolling normalized permutation entropy (KS proxy) over closes: writes
-- `count` doubles into out (NaN until the window fills), returns count on
-- success, 0 on invalid input
foreign export ccall "sim_perm_entropy" simPermEntropy
  :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt

simPermEntropy :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt
simPermEntropy ptr (CInt count) (CInt win) (CInt m) out
  | count <= 0 || win <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let vals = rollingPermEntropy (fromIntegral win) (fromIntegral m) (map (\(CDouble d) -> d) closes)
      if length vals /= fromIntegral count
        then pure 0
        else do
          pokeArray out (map CDouble vals)
          pure (CInt (fromIntegral count))

-- rolling Complexity-Entropy Causality Plane over closes: writes
-- 2 * `count` doubles into out — (H, C) interleaved per candle, NaN until
-- the window fills — returns 2 * count on success, 0 on invalid input
foreign export ccall "sim_cecp" simCecp
  :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt

simCecp :: Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt
simCecp ptr (CInt count) (CInt win) (CInt m) out
  | count <= 0 || win <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let pairs = rollingCECP (fromIntegral win) (fromIntegral m) (map (\(CDouble d) -> d) closes)
          vals = concatMap (\(h, c) -> [CDouble h, CDouble c]) pairs
      if length pairs /= fromIntegral count
        then pure 0
        else do
          pokeArray out vals
          pure (CInt (fromIntegral (length vals)))

-- simple moving average over closes: writes `count` doubles into out
-- (NaN until the window fills), returns count on success, 0 on invalid
-- input
foreign export ccall "sim_sma" simSma
  :: Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> IO CInt

simSma :: Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> IO CInt
simSma ptr (CInt count) (CInt period) out
  | count <= 0 || period <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let vals = VU.toList (smaSeries (fromIntegral period) (VU.fromList (map (\(CDouble d) -> d) closes)))
      if length vals /= fromIntegral count
        then pure 0
        else do
          pokeArray out (map CDouble vals)
          pure (CInt (fromIntegral count))

-- MACD over closes: writes 3 * `count` doubles into out — (macd, signal,
-- hist) interleaved per candle, NaN warm-up — returns 3 * count on
-- success, 0 on invalid input
foreign export ccall "sim_macd" simMacd
  :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt

simMacd :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt
simMacd ptr (CInt count) (CInt fast) (CInt slow) (CInt sig) out
  | count <= 0 || fast <= 0 || slow <= 0 || sig <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let rows = macdSeries (fromIntegral fast) (fromIntegral slow) (fromIntegral sig)
            (VU.fromList (map (\(CDouble d) -> d) closes))
          vals = concatMap (\(m, s, h) -> [CDouble m, CDouble s, CDouble h]) (VU.toList rows)
      if VU.length rows /= fromIntegral count
        then pure 0
        else do
          pokeArray out vals
          pure (CInt (fromIntegral (length vals)))

-- Wilder's RSI over closes: writes `count` doubles into out (NaN for
-- i < period), returns count on success, 0 on invalid input
foreign export ccall "sim_rsi" simRsi
  :: Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> IO CInt

simRsi :: Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> IO CInt
simRsi ptr (CInt count) (CInt period) out
  | count <= 0 || period <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let vals = VU.toList (rsiSeries (fromIntegral period) (VU.fromList (map (\(CDouble d) -> d) closes)))
      if length vals /= fromIntegral count
        then pure 0
        else do
          pokeArray out (map CDouble vals)
          pure (CInt (fromIntegral count))

-- Bollinger bands over closes: writes 3 * `count` doubles into out —
-- (mid, upper, lower) interleaved per candle, NaN warm-up — returns
-- 3 * count on success, 0 on invalid input
foreign export ccall "sim_bb" simBb
  :: Ptr CDouble -> CInt -> CInt -> CDouble -> Ptr CDouble -> IO CInt

simBb :: Ptr CDouble -> CInt -> CInt -> CDouble -> Ptr CDouble -> IO CInt
simBb ptr (CInt count) (CInt period) (CDouble mult) out
  | count <= 0 || period <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let rows = bollingerSeries (fromIntegral period) mult
            (VU.fromList (map (\(CDouble d) -> d) closes))
          vals = concatMap (\(m, u, l) -> [CDouble m, CDouble u, CDouble l]) (VU.toList rows)
      if VU.length rows /= fromIntegral count
        then pure 0
        else do
          pokeArray out vals
          pure (CInt (fromIntegral (length vals)))

-- rolling Bull-Sideways-Bear probabilities + surprise-adaptive 3x3 Markov
-- transition matrix over PRE-CLASSIFIED states: writes 15 * `count`
-- doubles into out — per bar [bsbpBull, bsbpSw, bsbpBear, t00..t22
-- (row-major), surprise, rolling surprise, lambda], NaN warm-up on BSBP
-- — returns 15 * count on success, 0 on invalid input.
-- `states` holds one state per slice bar (0 Bull, 1 Sideways, 2 Bear;
-- index 0 an unused dummy, states from index 1 on) — the caller
-- classifies the WHOLE loaded slice (window sums, see Sim.Markov.statesOf)
-- so the states are exact for any subslice; only the count/surprise
-- chain restarts at the slice start (uniform prior). lamHi/lamLo/sRef/k
-- sanitize into ranges (see Sim.Markov.sanitizeParams)
foreign export ccall "sim_markov" simMarkov
  :: Ptr CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO CInt

simMarkov :: Ptr CInt -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO CInt
simMarkov ptr (CInt count) (CInt win) (CInt surpWin) (CDouble lamHi) (CDouble lamLo) (CDouble sRef) (CDouble k) out
  | count <= 0 || win <= 0 = pure 0
  | otherwise = do
      sts <- peekArray (fromIntegral count) ptr
      -- mpWeighted is irrelevant here: the caller classifies (states
      -- arrive pre-computed), only the chain params matter
      let mp = MarkovParams (fromIntegral win) (fromIntegral surpWin) 5.0 lamHi lamLo sRef k False
          vals = markovSeriesStates mp (VU.fromList (map (\(CInt x) -> fromIntegral x) sts))
      if length vals /= 15 * fromIntegral count
        then pure 0
        else do
          pokeArray out (map CDouble vals)
          pure (CInt (fromIntegral (length vals)))

{-# NOINLINE progressRef #-}
progressRef :: IORef (Int, Int)
progressRef = unsafePerformIO (newIORef (0, 0))

{-# NOINLINE cancelRef #-}
cancelRef :: IORef Bool
cancelRef = unsafePerformIO (newIORef False)

simProgress :: IO CDouble
simProgress = do
  (done, total) <- readIORef progressRef
  pure (CDouble (if total <= 0 then 0 else fromIntegral done / fromIntegral total))

simCancel :: IO ()
simCancel = writeIORef cancelRef True

simHooks :: SimHooks
simHooks = SimHooks
  { hookProgress = \d t -> writeIORef progressRef (d, t)
  , hookCancelled = readIORef cancelRef
  }

simGenerate :: CString -> CString -> IO CInt
simGenerate paramsPtr outPtr = do
  paramsStr <- peekCString paramsPtr
  outPath   <- peekCString outPtr
  case applyArgs (toArgs paramsStr) of
    Left _  -> pure 1
    Right p -> do
      writeIORef progressRef (0, spCandles p + 1)
      writeIORef cancelRef False
      result <- try (Run.generateCsv simHooks outPath p) :: IO (Either SomeException ())
      case result of
        Left err
          | Just SimCancelled <- fromException err -> do
              Run.cleanupOutput outPath p
              pure 3
          | otherwise -> pure 2
        Right _ -> do
          writeIORef progressRef (1, 1)
          pure 0

simContinue :: CString -> CString -> IO CInt
simContinue paramsPtr outPtr = do
  paramsStr <- peekCString paramsPtr
  outPath   <- peekCString outPtr
  case applyArgs (toArgs paramsStr) of
    Left _      -> pure 1
    Right pForm -> do
      st <- readState (statePath outPath) `catch` onMissing
      case st of
        Left _            -> pure 4
        Right (p0, n, gs) -> do
          let p = p0 { spCandles = spCandles pForm }
          writeIORef progressRef (0, spCandles p + 1)
          writeIORef cancelRef False
          sizes <- Run.newSidecarSizes
          Run.recordSidecarSizes outPath sizes
          result <- try (Run.appendCsvRes simHooks outPath p n gs) :: IO (Either SomeException (GenState, Int))
          case result of
            Left err
              | Just SimCancelled <- fromException err -> do
                  Run.restoreSidecars outPath p sizes
                  pure 3
              | otherwise -> pure 2
            Right (gs', added) -> do
              writeState (statePath outPath) p (n + added) gs'
              writeIORef progressRef (1, 1)
              pure 0
  where
    onMissing e
      | isDoesNotExistError e = pure (Left "no state")
      | otherwise             = ioError e

-- --- ML training FFI -------------------------------------------------------

-- start a batch of parallel training sessions on one dataset. The call
-- BLOCKS until all sessions finish (run it on a worker thread); progress
-- is polled via sim_ml_progress and the per-session table via
-- sim_ml_sessions. Each session trains on seed base+k and writes
-- <out>/ml_<seed>.json (metrics) and <out>/equity_<seed>.csv (the test
-- equity curve in candles csv format, plottable by the chart).
--
-- params (key=value pairs separated by ';'):
--   data=<path>          candles csv (sim or real-quotes format) or a
--                        binary .rbin store (RELQ/RELS) — required
--   out=<dir>            results directory — required
--   sessions=N           parallel sessions, seeds seed..seed+N-1 (default 4)
--   seed=N               base seed (default 1)
--   epochs=N             (default 50)
--   lr=D                 learning rate (default 0.05)
--   batch=N              mini-batch size (default 256)
--   hidden="16,8"        hidden layer sizes (default "16")
--   trainpct=D           train segment fraction (default 0.6)
--   valpct=D             validation segment fraction (default 0.2)
--   fee=D                per position change, fraction (default 0.0002)
--   lags="1,5,20"        feature return lags
--   volwin=N             realized vol window (default 20)
--   rangewin=N           range-envelope window (default 20)
--   smawin=N             SMA distance window (default 20)
--   volsma=N             volume SMA window (default 20)
--   kswin=N              permutation entropy window (0 = off, default 0)
--   ksm=N                ordinal pattern length (default 5)
--   labeleps=D           |next return| below = flat class (default 1e-5)
--
-- returns 0 on success (all sessions completed, some may be cancelled),
-- 1 on bad params, 2 on data/IO failure
foreign export ccall "sim_ml_train" simMlTrain :: CString -> IO CInt

-- overall batch progress 0..1 (sum of epoch fractions across sessions)
foreign export ccall "sim_ml_progress" simMlProgress :: IO CDouble

-- live session table as a JSON array string (caller frees nothing; the
-- buffer is freshly allocated per call and owned by the caller — the
-- shim copies it into a godot::String and frees it)
foreign export ccall "sim_ml_sessions" simMlSessions :: IO CString

-- request cancellation of the in-flight batch (idempotent)
foreign export ccall "sim_ml_cancel" simMlCancel :: IO ()

-- --- DTW correlation scan FFI ------------------------------------------------

-- start a batch of parallel DTW scan sessions. Single-pair mode: series A
-- (tf atf) vs series B (tf btf). Grid mode (tfs= given): adata/datab are
-- comma-separated paths aligned with the tfs list (empty entry = skip that
-- timeframe) and every non-empty tfA x tfB combination becomes a session.
-- The call BLOCKS until all sessions finish (run it on a worker thread);
-- progress is polled via sim_dtw_progress and the per-session table via
-- sim_dtw_sessions. Each session writes <out>/dtw_<tfA>_<tfB>_<win>.json
-- (summary + best match) and <out>/dtw_<tfA>_<tfB>_<win>.csv (position
-- line: one row per stepped A window).
--
-- params (key=value pairs separated by ';'):
--   dataa=<path|list>    candles csv (sim or real-quotes) or .rbin — required
--   datab=<path|list>    same for series B — required
--   out=<dir>            results directory — required
--   atf=<label>          timeframe label of series A (single mode, default "A")
--   btf=<label>          timeframe label of series B (single mode, default "B")
--   tfs="1m,5m,..."      grid mode: timeframe labels aligned with the lists
--   windows="64,128"     window lengths (default "128")
--   band=N               Sakoe-Chiba band (default win/4 per window)
--   step=N               A-window slide step (default win/4 per window)
--   maxcand=N            candidate verifications cap per window (default 64)
--   paa=N                fingerprint segments (default 8, must divide win)
--   card=N               quantization cardinality 3..8 (default 8)
--
-- returns 0 on success (all sessions completed, some may be cancelled),
-- 1 on bad params, 2 on data/IO failure, 3 when cancelled
foreign export ccall "sim_dtw_scan" simDtwScan :: CString -> IO CInt

foreign export ccall "sim_dtw_progress" simDtwProgress :: IO CDouble

foreign export ccall "sim_dtw_sessions" simDtwSessions :: IO CString

foreign export ccall "sim_dtw_cancel" simDtwCancel :: IO ()

-- drill-down for one A window: banded free-end DTW against the B segment
-- starting at j (pass the j from the matches csv). Returns a JSON string:
-- {"dist":D,"win":W,"seg":S,"a":[z-normalized query],
--  "b":[z-normalized segment],"path":[[q,r],...]} — b/path indices are
-- relative to j; "error" set on failure. Loaded closes are cached
-- per-process, so repeat drill-downs on the same files are cheap.
foreign export ccall "sim_dtw_path" simDtwPath :: CString -> IO CString

-- rebuild signal A from the best set of matched B chunks of a FINISHED
-- scan (second pass after scan/load). Writes <out>.rbin — a candle-only
-- RELS binary the chart page loads like any saved series — and <out>.json
-- with the segment table: for every stitched segment its A/B ranges and
-- the list of selected signal-B chunk indices ([42] single, [42,44,50]
-- stitched).
--
-- params (key=value pairs separated by ';'):
--   matches=<path>   RELM .bin of a scan session — required
--   dataa=<path>     series A closes (csv or RELQ/RELS rbin) — required
--   datab=<path>     series B closes — required
--   out=<path>       output prefix — required (gets .rbin + .json)
--   gap=N            max A gap (samples) that still chains chunks into one
--                    segment (default: the scan window)
--   tol=D            stitch re-validation tolerance, multiple of the
--                    median match distance (default 1.5)
--   wy=D             Y-deformation cost weight (default 1.0)
--   wp=D             periodic scaleX-deviation cost weight (default 0.5)
--   dec=N            RELS decimals header field (default 5)
--
-- returns a JSON summary: {"error":...,"covered":N,"total":N,
-- "segments":K} — the full per-segment table (with the selected B chunk
-- indices) is in the <out>.json artifact, not the summary
foreign export ccall "sim_dtw_build" simDtwBuild :: CString -> IO CString

-- --- strategy optimizer FFI ------------------------------------------------

-- start a batch of parallel SMA-cross strategy optimization sessions on
-- one dataset. The call BLOCKS until all sessions finish (run it on a
-- worker thread); progress is polled via sim_strat_progress and the
-- per-session table via sim_strat_sessions. Each session samples
-- `candidates` strategies on seed base+k and writes <out>/strat_<seed>.json
-- (the winning spec + metrics), <out>/equity_<seed>.csv (the test equity
-- curve in candles csv format), <out>/cand_<seed>.csv (val Calmar per
-- candidate) and <out>/test_<seed>.csv (close,equity,pos on the test
-- segment).
--
-- params (key=value pairs separated by ';'):
--   data=<path>          candles csv (sim or real-quotes format) or a
--                        binary .rbin store (RELQ/RELS) — required
--   out=<dir>            results directory — required
--   sessions=N           parallel sessions, seeds seed..seed+N-1 (default 4)
--   seed=N               base seed (default 1)
--   slots=N              rule slots per strategy (default 4)
--   candidates=N         strategies sampled per session (default 200)
--   valpct=D             val (search) segment fraction (default 0.2)
--   fee=D                per position change, fraction (default 0.0002)
--   fastmax=N            fastest SMA period upper bound (default 50)
--   slowmax=N            slowest SMA period upper bound (default 200)
--
-- returns 0 on success, 1 on bad params, 2 on data/IO failure, 3 when
-- cancelled (partial artifacts stay, like the ML trainer)
foreign export ccall "sim_strat_optimize" simStratOptimize :: CString -> IO CInt

foreign export ccall "sim_strat_progress" simStratProgress :: IO CDouble

foreign export ccall "sim_strat_sessions" simStratSessions :: IO CString

foreign export ccall "sim_strat_cancel" simStratCancel :: IO ()

-- backtest one hand-authored DSL strategy (Sim.Strat.Dsl) over a dataset
-- and write the result in the SAME artifact layout the optimizer uses,
-- with seed 0: <out>/strat_0.json, equity_0.csv, cand_0.csv (a single
-- candidate row), test_0.csv. The UI shows the run as session #0.
--
-- params (key=value pairs separated by ';'):
--   data=<path>          candles csv (sim or real-quotes format) or a
--                        binary .rbin store (RELQ/RELS) — required
--   out=<dir>            results directory — required
--   dsl=<spec>           strategy text; rules separated by newlines
--                        (';' also works outside this params context)
--   fee=D                per position change, fraction (default 0.0002)
--
-- returns 0 on success, 1 on bad params/dsl parse, 2 on data/IO failure
foreign export ccall "sim_strat_dsl" simStratDsl :: CString -> IO CInt

data StratSessionState = StratSessionState
  { ssSeed   :: !Word64
  , ssCand   :: !(IORef Int)
  , ssCands  :: !Int
  , ssBest   :: !(IORef Double)
  , ssStatus :: !(IORef String)
  }

data StratParams = StratParams
  { stpData     :: !FilePath
  , stpOut      :: !FilePath
  , stpSessions :: !Int
  , stpSeedBase :: !Word64
  , stpOpt      :: !STO.OptParams
  }

{-# NOINLINE stratProgressRef #-}
stratProgressRef :: IORef (Int, Int)
stratProgressRef = unsafePerformIO (newIORef (0, 0))

{-# NOINLINE stratCancelRef #-}
stratCancelRef :: IORef Bool
stratCancelRef = unsafePerformIO (newIORef False)

{-# NOINLINE stratSessionsRef #-}
stratSessionsRef :: IORef [StratSessionState]
stratSessionsRef = unsafePerformIO (newIORef [])

simStratProgress :: IO CDouble
simStratProgress = do
  (done, total) <- readIORef stratProgressRef
  pure (CDouble (if total <= 0 then 0 else fromIntegral done / fromIntegral total))

simStratCancel :: IO ()
simStratCancel = writeIORef stratCancelRef True

simStratSessions :: IO CString
simStratSessions = do
  ss <- readIORef stratSessionsRef
  parts <- mapM sessionJson ss
  newCString ("[" ++ intercalate "," parts ++ "]")
  where
    sessionJson st = do
      c <- readIORef (ssCand st)
      b <- readIORef (ssBest st)
      s <- readIORef (ssStatus st)
      let b' = if isInfinite b || isNaN b then 0 else b
      pure $ "{\"seed\":" ++ show (ssSeed st)
          ++ ",\"candidate\":" ++ show c
          ++ ",\"candidates\":" ++ show (ssCands st)
          ++ ",\"best\":" ++ show b'
          ++ ",\"status\":\"" ++ s ++ "\"}"

simStratOptimize :: CString -> IO CInt
simStratOptimize paramsPtr = do
  paramsStr <- peekCString paramsPtr
  case parseStratParams paramsStr of
    Left _  -> pure 1
    Right p -> do
      writeIORef stratCancelRef False
      result <- try (runStratBatch p) :: IO (Either SomeException ())
      case result of
        Left err
          | Just SimCancelled <- fromException err -> pure 3
          | otherwise                            -> pure 2
        Right _ -> pure 0

simStratDsl :: CString -> IO CInt
simStratDsl paramsPtr = do
  paramsStr <- peekCString paramsPtr
  case parseDslParams paramsStr of
    Left _  -> pure 1
    Right p -> do
      result <- try (Run.runDslBacktest (dspData p) (dspOut p) (dspSpec p) (dspFee p)) :: IO (Either SomeException ())
      case result of
        Left _    -> pure 2
        Right _   -> pure 0

data DslParams = DslParams
  { dspData :: !FilePath
  , dspOut  :: !FilePath
  , dspSpec :: !STD.DslStrat
  , dspFee  :: !Double
  }

parseDslParams :: String -> Either String DslParams
parseDslParams s = do
  let kv = kvPairs s
  dataPath <- maybe (Left "missing data=") Right (lookup "data" kv)
  outDir   <- maybe (Left "missing out=") Right (lookup "out" kv)
  specS    <- maybe (Left "missing dsl=") Right (lookup "dsl" kv)
  spec     <- STD.parseDsl specS
  fee      <- maybe (Right 0.0002) readD (lookup "fee" kv)
  pure (DslParams dataPath outDir spec fee)
  where
    readD v = maybe (Left "bad fee") Right (readV v)
    readV :: Read a => String -> Maybe a
    readV v = case reads v of
      [(x, "")] -> Just x
      _         -> Nothing

runStratBatch :: StratParams -> IO ()
runStratBatch p = do
  let cands = max 1 (STO.opCandidates (stpOpt p))
      n = max 1 (stpSessions p)
  stRefs <- forM [0 .. n - 1] $ \k -> do
    c <- newIORef 0
    b <- newIORef (-1 / 0)
    s <- newIORef "run"
    pure (StratSessionState (stpSeedBase p + fromIntegral k) c cands b s)
  writeIORef stratSessionsRef stRefs
  writeIORef stratProgressRef (0, n * cands)
  let sessions =
        [ (ssSeed st, STO.OptHooks
            { STO.ohProgress = \c _ -> do
                writeIORef (ssCand st) c
                _ <- atomicModifyIORef' stratProgressRef (\(d, t) -> ((d + 1, t), ()))
                pure ()
            , STO.ohBest = \cm -> do
                best <- readIORef (ssBest st)
                when (cm > best) (writeIORef (ssBest st) cm)
            , STO.ohCancelled = readIORef stratCancelRef
            })
        | st <- stRefs
        ]
  Run.runStratBatch (stpOpt p) (stpData p) (stpOut p) sessions
  -- reflect per-session completion in the status table (cancelled/error
  -- sessions simply produced no artifacts)
  forM stRefs $ \st -> do
    done <- doesFileExist (stpOut p ++ "/strat_" ++ show (ssSeed st) ++ ".json")
    writeIORef (ssStatus st) (if done then "done" else "cancelled")
  pure ()

parseStratParams :: String -> Either String StratParams
parseStratParams s = do
  let kv = kvPairs s
  dataPath <- maybe (Left "missing data=") Right (lookup "data" kv)
  outDir   <- maybe (Left "missing out=") Right (lookup "out" kv)
  sessions <- readK "sessions" 4
  seedBase <- readK "seed" (1 :: Word64)
  slots    <- readK "slots" 4
  cands    <- readK "candidates" 200
  valPct   <- readKD "valpct" 0.2
  fee      <- readKD "fee" 0.0002
  fastMax  <- readK "fastmax" 50
  slowMax  <- readK "slowmax" 200
  let opt = STO.defaultOptParams
        { STO.opSlots = slots
        , STO.opCandidates = cands
        , STO.opValFrac = valPct
        , STO.opFee = fee
        , STO.opFastMax = fastMax
        , STO.opSlowMax = slowMax
        }
  pure (StratParams dataPath outDir sessions seedBase opt)
  where
    readK :: Read a => String -> a -> Either String a
    readK k d = maybe (Right d) (readTK k) (lookup k (kvPairs s))
    readKD :: String -> Double -> Either String Double
    readKD k d = maybe (Right d) (readTK k) (lookup k (kvPairs s))
    readTK :: Read a => String -> String -> Either String a
    readTK k v = maybe (Left ("bad " ++ k)) Right (readV v)
    readV :: Read a => String -> Maybe a
    readV v = case reads v of
      [(x, "")] -> Just x
      _         -> Nothing

data MlSessionState = MlSessionState
  { msSeed   :: !Word64
  , msEpoch  :: !(IORef Int)
  , msEpochs :: !Int
  , msBest   :: !(IORef Double)
  , msStatus :: !(IORef String)
  }

data MlParams = MlParams
  { mlpData     :: !FilePath
  , mlpOut      :: !FilePath
  , mlpSessions :: !Int
  , mlpSeedBase :: !Word64
  , mlpTrain    :: !MLT.TrainParams
  }

{-# NOINLINE mlProgressRef #-}
mlProgressRef :: IORef (Int, Int)
mlProgressRef = unsafePerformIO (newIORef (0, 0))

{-# NOINLINE mlCancelRef #-}
mlCancelRef :: IORef Bool
mlCancelRef = unsafePerformIO (newIORef False)

{-# NOINLINE mlSessionsRef #-}
mlSessionsRef :: IORef [MlSessionState]
mlSessionsRef = unsafePerformIO (newIORef [])

simMlProgress :: IO CDouble
simMlProgress = do
  (done, total) <- readIORef mlProgressRef
  pure (CDouble (if total <= 0 then 0 else fromIntegral done / fromIntegral total))

simMlCancel :: IO ()
simMlCancel = writeIORef mlCancelRef True

simMlSessions :: IO CString
simMlSessions = do
  ss <- readIORef mlSessionsRef
  parts <- mapM sessionJson ss
  newCString ("[" ++ intercalate "," parts ++ "]")
  where
    sessionJson st = do
      e <- readIORef (msEpoch st)
      b <- readIORef (msBest st)
      s <- readIORef (msStatus st)
      let b' = if isInfinite b || isNaN b then 0 else b
      pure $ "{\"seed\":" ++ show (msSeed st)
          ++ ",\"epoch\":" ++ show e
          ++ ",\"epochs\":" ++ show (msEpochs st)
          ++ ",\"best\":" ++ show b'
          ++ ",\"status\":\"" ++ s ++ "\"}"

simMlTrain :: CString -> IO CInt
simMlTrain paramsPtr = do
  paramsStr <- peekCString paramsPtr
  case parseMlParams paramsStr of
    Left _  -> pure 1
    Right p -> do
      writeIORef mlCancelRef False
      result <- try (runMlBatch p) :: IO (Either SomeException ())
      case result of
        Left err
          | Just SimCancelled <- fromException err -> pure 3
          | otherwise                            -> pure 2
        Right _ -> pure 0

runMlBatch :: MlParams -> IO ()
runMlBatch p = do
  let epochs = MLT.tpEpochs (mlpTrain p)
      n = max 1 (mlpSessions p)
  stRefs <- forM [0 .. n - 1] $ \k -> do
    e <- newIORef 0
    b <- newIORef (-1 / 0)
    s <- newIORef "run"
    pure (MlSessionState (mlpSeedBase p + fromIntegral k) e epochs b s)
  writeIORef mlSessionsRef stRefs
  writeIORef mlProgressRef (0, n * epochs)
  let sessions =
        [ (msSeed st, MLT.TrainHooks
            { MLT.thProgress = \e _ -> do
                writeIORef (msEpoch st) e
                _ <- atomicModifyIORef' mlProgressRef (\(d, t) -> ((d + 1, t), ()))
                pure ()
            , MLT.thBest = \vc -> do
                best <- readIORef (msBest st)
                when (vc > best) (writeIORef (msBest st) vc)
            , MLT.thCancelled = readIORef mlCancelRef
            })
        | st <- stRefs
        ]
  Run.runMlBatch (mlpTrain p) (mlpData p) (mlpOut p) sessions
  forM stRefs $ \st -> do
    done <- doesFileExist (mlpOut p ++ "/ml_" ++ show (msSeed st) ++ ".json")
    writeIORef (msStatus st) (if done then "done" else "cancelled")
  pure ()

-- --- DTW scan implementation --------------------------------------------------

data DtwSessionState = DtwSessionState
  { dsTfA     :: !String
  , dsTfB     :: !String
  , dsWin     :: !Int
  , dsWindows :: !Int
  , dsDone    :: !(IORef Int)
  , dsBest    :: !(IORef Double)
  , dsStatus  :: !(IORef String)
  }

{-# NOINLINE dtwProgressRef #-}
dtwProgressRef :: IORef (Int, Int)
dtwProgressRef = unsafePerformIO (newIORef (0, 0))

{-# NOINLINE dtwCancelRef #-}
dtwCancelRef :: IORef Bool
dtwCancelRef = unsafePerformIO (newIORef False)

{-# NOINLINE dtwSessionsRef #-}
dtwSessionsRef :: IORef [DtwSessionState]
dtwSessionsRef = unsafePerformIO (newIORef [])

simDtwProgress :: IO CDouble
simDtwProgress = do
  (done, total) <- readIORef dtwProgressRef
  pure (CDouble (if total <= 0 then 0 else min 1 (fromIntegral done / fromIntegral total)))

simDtwCancel :: IO ()
simDtwCancel = writeIORef dtwCancelRef True

simDtwSessions :: IO CString
simDtwSessions = do
  ss <- readIORef dtwSessionsRef
  parts <- mapM sessionJson ss
  newCString ("[" ++ intercalate "," parts ++ "]")
  where
    sessionJson st = do
      d <- readIORef (dsDone st)
      b <- readIORef (dsBest st)
      s <- readIORef (dsStatus st)
      let b' = if isInfinite b || b /= b then 0 else b
      pure $ "{\"tfA\":\"" ++ dsTfA st ++ "\""
          ++ ",\"tfB\":\"" ++ dsTfB st ++ "\""
          ++ ",\"win\":" ++ show (dsWin st)
          ++ ",\"windows\":" ++ show (dsWindows st)
          ++ ",\"done\":" ++ show d
          ++ ",\"best\":" ++ show b'
          ++ ",\"status\":\"" ++ s ++ "\"}"

-- adata/bdata hold one path per timeframe entry; entries may be empty
-- (timeframe skipped). Grid mode = several non-empty entries.
data DtwParams = DtwParams
  { dpDataA   :: ![FilePath]
  , dpDataB   :: ![FilePath]
  , dpTFsA    :: ![String]
  , dpTFsB    :: ![String]
  , dpOut     :: !FilePath
  , dpWins    :: ![Int]
  , dpBandF   :: !Int
  , dpStepF   :: !Int
  , dpMaxC    :: !Int
  , dpPaaN    :: !Int
  , dpCardN   :: !Int
  }

simDtwScan :: CString -> IO CInt
simDtwScan paramsPtr = do
  paramsStr <- peekCString paramsPtr
  case parseDtwParams paramsStr of
    Left _  -> pure 1
    Right p -> do
      writeIORef dtwCancelRef False
      result <- try (runDtwScanBatch p) :: IO (Either SomeException ())
      case result of
        Left err
          | Just SimCancelled <- fromException err -> pure 3
          | otherwise                            -> pure 2
        Right _ -> pure 0

runDtwScanBatch :: DtwParams -> IO ()
runDtwScanBatch p = do
  cacheRef <- newIORef Map.empty
  loadedA <- mapM (loadPrefix cacheRef) (dpDataA p)
  loadedB <- mapM (loadPrefix cacheRef) (dpDataB p)
  let pairsA = catMaybes (zipWith (\tf mv -> fmap (\v -> (tf, v)) mv) (dpTFsA p) loadedA)
      pairsB = catMaybes (zipWith (\tf mv -> fmap (\v -> (tf, v)) mv) (dpTFsB p) loadedB)
  if null pairsA || null pairsB
    then ioError (userError "dtw: no usable series in dataa/datab")
    else do
      let plans = Run.dtwJobs pairsA pairsB (dpWins p) (dpBandF p) (dpStepF p) (dpMaxC p) (dpPaaN p) (dpCardN p)
          planSp pl = DTW.ScanParams (Run.dpWin pl) (Run.dpBand pl) (Run.dpPaa pl) (Run.dpCard pl) (Run.dpStep pl) (Run.dpMaxCand pl)
          planNWin (pl, pfa, pfb) =
            if DTW.scanValid (planSp pl) (pfLen pfa) (pfLen pfb)
              then length [0, Run.dpStep pl .. pfLen pfa - Run.dpWin pl]
              else 0
      stRefs <- forM plans $ \job@(pl, _, _) -> do
        d <- newIORef 0
        b <- newIORef (1 / 0)
        s <- newIORef "run"
        pure (DtwSessionState (Run.dpTfA pl) (Run.dpTfB pl) (Run.dpWin pl) (planNWin job) d b s, job)
      writeIORef dtwSessionsRef (map fst stRefs)
      writeIORef dtwProgressRef (0, sum (map (\(st, _) -> dsWindows st) stRefs))
      let mkHooks st = Run.DtwHooks
            { Run.dwProgress = \d _ -> do
                writeIORef (dsDone st) d
                _ <- atomicModifyIORef' dtwProgressRef (\(done, tot) -> ((done + 1, tot), ()))
                pure ()
            , Run.dwBest = \best -> do
                cur <- readIORef (dsBest st)
                when (best < cur) (writeIORef (dsBest st) best)
            , Run.dwCancelled = readIORef dtwCancelRef
            }
          jobs = [ (pl, pfa, pfb, mkHooks st) | (st, (pl, pfa, pfb)) <- stRefs ]
      Run.runDtwBatch (dpOut p) jobs
      forM stRefs $ \(st, (pl, _, _)) -> do
        done <- doesFileExist (dpOut p ++ "/dtw_" ++ Run.dpTfA pl ++ "_" ++ Run.dpTfB pl ++ "_" ++ show (Run.dpWin pl) ++ ".json")
        writeIORef (dsStatus st) (if done then "done" else if dsWindows st == 0 then "short" else "cancelled")
      pure ()
  where
  loadPrefix :: IORef (Map.Map FilePath Prefix) -> FilePath -> IO (Maybe Prefix)
  loadPrefix _ "" = pure Nothing
  loadPrefix cacheRef path = do
    cached <- readIORef cacheRef
    case Map.lookup path cached of
      Just v  -> pure (Just v)
      Nothing -> do
        parsed <- MLD.loadCloses path
        case parsed of
          Left _     -> pure Nothing
          Right cls -> do
            let v = mkPrefix cls
            modifyIORef' cacheRef (Map.insert path v)
            pure (Just v)

-- --- DTW drill-down ----------------------------------------------------------

{-# NOINLINE dtwPathCacheRef #-}
dtwPathCacheRef :: IORef (Map.Map FilePath Prefix)
dtwPathCacheRef = unsafePerformIO (newIORef Map.empty)

simDtwPath :: CString -> IO CString
simDtwPath paramsPtr = do
  paramsStr <- peekCString paramsPtr
  out <- try (computeDtwPath paramsStr) :: IO (Either SomeException String)
  newCString (either (const "{\"error\":\"exception\"}") id out)

computeDtwPath :: String -> IO String
computeDtwPath s = do
  let kv = kvPairs s
      look k = lookup k kv
      readI k d = fromMaybe d (look k >>= readV)
      readV v = case reads v of
        [(x, "")] -> Just (x :: Int)
        _         -> Nothing
  case (look "dataa", look "datab", look "i", look "j") of
    (Just pa, Just pb, Just iStr, Just jStr) -> do
      let mI = readV iStr
          mJ = readV jStr
          win = readI "win" 128
          band = max 1 (readI "band" (max 1 (win `div` 4)))
      case (mI, mJ) of
        (Just i, Just j) | j >= 0, win > 0 -> do
          mpa <- cachedPrefix pa
          mpb <- cachedPrefix pb
          case (mpa, mpb) of
            (Just pfa, Just pfb)
              | i >= 0, i + win <= pfLen pfa, j + win + 2 * band <= pfLen pfb -> do
                  let q = zslice pfa i win
                      seg = zwin pfb j win (win + 2 * band)
                      (cost, path) = dtwPathFree q seg band
                      dist = stepCost (cost, path)
                  pure $ jsonPath win (win + 2 * band) dist q seg path
            _ -> pure "{\"error\":\"bad params or data\"}"
        _ -> pure "{\"error\":\"bad params\"}"
    _ -> pure "{\"error\":\"missing params\"}"
  where
  cachedPrefix path = do
    cached <- readIORef dtwPathCacheRef
    case Map.lookup path cached of
      Just v  -> pure (Just v)
      Nothing -> do
        parsed <- MLD.loadCloses path
        case parsed of
          Left _    -> pure Nothing
          Right cls -> do
            let v = mkPrefix cls
            modifyIORef' dtwPathCacheRef (Map.insert path v)
            pure (Just v)
  jsonPath win segLen dist q seg path =
    let dbls v = "[" ++ intercalate "," (map show (VU.toList v)) ++ "]"
        pairs = "[" ++ intercalate "," [ "[" ++ show a ++ "," ++ show b ++ "]" | (a, b) <- path ] ++ "]"
    in "{\"dist\":" ++ show dist
       ++ ",\"win\":" ++ show win
       ++ ",\"seg\":" ++ show segLen
       ++ ",\"a\":" ++ dbls q
       ++ ",\"b\":" ++ dbls seg
       ++ ",\"path\":" ++ pairs
       ++ "}"

-- --- DTW reconstruction (second pass) ---------------------------------------

simDtwBuild :: CString -> IO CString
simDtwBuild paramsPtr = do
  paramsStr <- peekCString paramsPtr
  out <- try (computeDtwBuild paramsStr) :: IO (Either SomeException String)
  newCString (either (const "{\"error\":\"exception\"}") id out)

computeDtwBuild :: String -> IO String
computeDtwBuild s = do
  let kv = kvPairs s
      look k = lookup k kv
      readI k d = fromMaybe d (look k >>= readV)
      readD k d = fromMaybe d (look k >>= readVD)
      readV v = case reads v of
        [(x, "")] -> Just (x :: Int)
        _         -> Nothing
      readVD v = case reads v of
        [(x, "")] -> Just (x :: Double)
        _         -> Nothing
  case (look "matches", look "dataa", look "datab", look "out") of
    (Just mPath, Just pa, Just pb, Just out) -> do
      eHdr <- parseMatchesHdr <$> B.readFile mPath
      case eHdr of
        Left err -> pure ("{\"error\":\"" ++ err ++ "\"}")
        Right (win, band, _, ms) -> do
          mpa <- cachedPrefix pa
          mpb <- cachedPrefix pb
          case (mpa, mpb) of
            (Just pfa, Just pfb) -> do
              let se0 = SEL.defaultSelParams
                  se = se0 { SEL.seGap = let g = readI "gap" 0 in if g <= 0 then win else g
                           , SEL.seTol = readD "tol" (SEL.seTol se0)
                           , SEL.seWy = readD "wy" (SEL.seWy se0)
                           , SEL.seWp = readD "wp" (SEL.seWp se0) }
                  dec = readI "dec" 5
                  rb = SEL.selectRebuild se pfa pfb win band ms
              if VU.null (SEL.rbCloses rb)
                then pure "{\"error\":\"no usable matches\"}"
                else do
                  BL.writeFile (out ++ ".rbin") (toLazyByteString (relsBin dec (SEL.rbCloses rb)))
                  let meta = buildMeta pa pb win band se rb
                  writeFile (out ++ ".json") meta
                  pure (buildSummary rb)
            _ -> pure "{\"error\":\"bad params or data\"}"
    _ -> pure "{\"error\":\"missing params\"}"
  where
  cachedPrefix path = do
    cached <- readIORef dtwPathCacheRef
    case Map.lookup path cached of
      Just v  -> pure (Just v)
      Nothing -> do
        parsed <- MLD.loadCloses path
        case parsed of
          Left _    -> pure Nothing
          Right cls -> do
            let v = mkPrefix cls
            modifyIORef' dtwPathCacheRef (Map.insert path v)
            pure (Just v)
  segJson s =
    "{\"a0\":" ++ show (SEL.sgA0 s)
      ++ ",\"a1\":" ++ show (SEL.sgA1 s)
      ++ ",\"b0\":" ++ show (SEL.sgB0 s)
      ++ ",\"b1\":" ++ show (SEL.sgB1 s)
      ++ ",\"dist\":" ++ show (SEL.sgDist s)
      ++ ",\"scaleX\":" ++ show (SEL.sgScaleX s)
      ++ ",\"chunks\":[" ++ intercalate "," (map (show . SEL.chJ) (SEL.sgChunks s)) ++ "]}"
  segsJson rb = "[" ++ intercalate "," (map segJson (SEL.rbSegs rb)) ++ "]"
  buildSummary rb =
    "{\"error\":null,\"covered\":" ++ show (SEL.rbCovered rb)
      ++ ",\"total\":" ++ show (VU.length (SEL.rbCloses rb))
      ++ ",\"segments\":" ++ show (length (SEL.rbSegs rb)) ++ "}"
  buildMeta pa pb win band se rb =
    "{\"dataa\":\"" ++ esc pa ++ "\",\"datab\":\"" ++ esc pb ++ "\""
      ++ ",\"win\":" ++ show win
      ++ ",\"band\":" ++ show band
      ++ ",\"gap\":" ++ show (SEL.seGap se)
      ++ ",\"tol\":" ++ show (SEL.seTol se)
      ++ ",\"covered\":" ++ show (SEL.rbCovered rb)
      ++ ",\"total\":" ++ show (VU.length (SEL.rbCloses rb))
      ++ ",\"segments\":" ++ segsJson rb ++ "}"
  esc = concatMap (\c -> if c == '"' || c == '\\' then '\\' : [c] else [c])

parseDtwParams :: String -> Either String DtwParams
parseDtwParams s = do
  let kv = kvPairs s
      look k = lookup k kv
      splitCsv v = map trim (split ',' v)
  dataA  <- maybe (Left "missing dataa=") Right (look "dataa")
  dataB  <- maybe (Left "missing datab=") Right (look "datab")
  outDir <- maybe (Left "missing out=") Right (look "out")
  (tfsA, tfsB, pathsA, pathsB) <- case look "tfs" of
    Just tv -> do
      let tfList = splitCsv tv
          pa = splitCsv dataA
          pb = splitCsv dataB
      if length pa == length tfList && length pb == length tfList
        then Right (tfList, tfList, pa, pb)
        else Left "tfs/dataa/datab length mismatch"
    Nothing ->
      let tfA = fromMaybe "A" (look "atf")
          tfB = fromMaybe "B" (look "btf")
      in Right ([tfA], [tfB], [dataA], [dataB])
  wins   <- case look "windows" of
    Nothing -> Right [128]
    Just v | Just ws <- mapM readInt (split ',' v), not (null ws), all (> 0) ws -> Right ws
           | otherwise -> Left "bad windows"
  band   <- readK "band" 0
  step   <- readK "step" 0
  maxc   <- readK "maxcand" 64
  paaN   <- readK "paa" 8
  cardN  <- readK "card" 8
  pure (DtwParams pathsA pathsB tfsA tfsB outDir wins band step maxc paaN cardN)
  where
  readK :: Read a => String -> a -> Either String a
  readK k d = case lookup k (kvPairs s) of
    Nothing -> Right d
    Just v  -> maybe (Left ("bad " ++ k)) Right (readV v)
  readV v = case reads v of
    [(x, "")] -> Just x
    _         -> Nothing
  readInt v = case reads v of
    [(x, "")] -> Just (x :: Int)
    _         -> Nothing

parseMlParams :: String -> Either String MlParams
parseMlParams s = do
  let kv = kvPairs s
      look k = lookup k kv
  dataPath <- maybe (Left "missing data=") Right (look "data")
  outDir   <- maybe (Left "missing out=") Right (look "out")
  sessions <- readK "sessions" 4
  seedBase <- readK "seed" (1 :: Word64)
  epochs   <- readK "epochs" 50
  lr       <- readKD "lr" 0.05
  batch    <- readK "batch" 256
  hidden   <- parseInts "hidden" [16] "16"
  trainPct <- readKD "trainpct" 0.6
  valPct   <- readKD "valpct" 0.2
  fee      <- readKD "fee" 0.0002
  lags     <- parseInts "lags" [1, 5, 20] "1,5,20"
  volWin   <- readK "volwin" 20
  rangeWin <- readK "rangewin" 20
  smaWin   <- readK "smawin" 20
  volSma   <- readK "volsma" 20
  ksWin    <- readK "kswin" 0
  ksM      <- readK "ksm" 5
  labelEps <- readKD "labeleps" 1e-5
  let spec = MLD.defaultFeatureSpec
        { MLD.fsLags = lags
        , MLD.fsVolWin = volWin
        , MLD.fsRangeWin = rangeWin
        , MLD.fsSmaWin = smaWin
        , MLD.fsVolSmaWin = volSma
        , MLD.fsKsWin = ksWin
        , MLD.fsKsM = ksM
        , MLD.fsLabelEps = labelEps
        }
  pure (MlParams dataPath outDir sessions seedBase (MLT.TrainParams epochs lr batch hidden trainPct valPct fee spec))
  where
    kv = kvPairs s
    readK :: Read a => String -> a -> Either String a
    readK k d = maybe (Right d) (readTK k) (lookup k (kvPairs s))
    readKD :: String -> Double -> Either String Double
    readKD k d = maybe (Right d) (readTK k) (lookup k (kvPairs s))
    readTK :: Read a => String -> String -> Either String a
    readTK k v = maybe (Left ("bad " ++ k)) Right (readV v)
    parseInts :: String -> [Int] -> String -> Either String [Int]
    parseInts k d fmt = case lookup k kv of
      Nothing -> Right d
      Just v | Just is <- mapM (readV :: String -> Maybe Int) (split ',' v), not (null is) -> Right is
             | otherwise -> Left ("bad " ++ k ++ " (expected " ++ fmt ++ ")")
    readV :: Read a => String -> Maybe a
    readV v = case reads v of
      [(x, "")] -> Just x
      _         -> Nothing

kvPairs :: String -> [(String, String)]
kvPairs = map pair . filter (not . null) . map trim . split ';'
  where
    pair kv = let (k, v) = break (== '=') kv in (k, drop 1 v)

toArgs :: String -> [String]
toArgs = concatMap pair . filter (not . null) . map trim . split ';'
  where
    pair kv =
      let (k, v) = break (== '=') kv
      in ["--" ++ k, drop 1 v]

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (pre, [])       -> [pre]
  (pre, _ : post) -> pre : split c post

trim :: String -> String
trim = dropWhile isSpace' . reverse . dropWhile isSpace' . reverse

isSpace' :: Char -> Bool
isSpace' ch = ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
