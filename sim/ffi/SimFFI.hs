module SimFFI where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, fromException, ioError, try)
import Control.Monad (forM, unless, when)
import qualified Data.ByteString as B
import Data.ByteString.Builder (char8, hPutBuilder, word64LE)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Sim.Cli (applyArgs)
import Sim.Csv (csvRowsText, csvText, lvlHeader, lvlRowB, levelsPath, metaText, tickHeader, tickRowB, ticksPath, tradesBin, vevHeader, vevPath, vevRecB)
import Sim.Entropy (rollingCECP, rollingPermEntropy)
import Sim.Fft (fftApprox, fftBest)
import Sim.Markov (MarkovParams (..), markovSeries)
import Sim.Gen (GenState (..), initState)
import Sim.GenH (runSimH)
import Sim.Lvl (LvlEv, LvlSeg, trFromBook)
import qualified Sim.ML.Data as MLD
import qualified Sim.ML.Train as MLT
import Sim.Resume (readState, statePath, writeState)
import Sim.Sinus (runSimSinusH)
import qualified Sim.Strat.Backtest as STB
import qualified Sim.Strat.Dsl as STD
import qualified Sim.Strat.Optimize as STO
import qualified Sim.Strat.Rules as STR
import Sim.TickGenH (runSimTicksH)
import Sim.Types (Candle, SimHooks (..), SimMode (..), SimParams (..), SimCancelled (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (AppendMode, ReadWriteMode, WriteMode), SeekMode (AbsoluteSeek, SeekFromEnd), hPutStr, hSeek, hSetBuffering, hSetFileSize, withFile)
import System.IO.Error (isDoesNotExistError, userError)
import Text.Printf (printf)

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

-- rolling Bull-Sideways-Bear probabilities + surprise-adaptive 3x3 Markov
-- transition matrix over closes: writes 15 * `count` doubles into out —
-- per bar [bsbpBull, bsbpSw, bsbpBear, t00..t22 (row-major), surprise,
-- rolling surprise, lambda], NaN warm-up on BSBP — returns 15 * count on
-- success, 0 on invalid input. thresh/lamHi/lamLo/sRef/k sanitize into
-- ranges (see Sim.Markov.sanitizeParams)
foreign export ccall "sim_markov" simMarkov
  :: Ptr CDouble -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO CInt

simMarkov :: Ptr CDouble -> CInt -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO CInt
simMarkov ptr (CInt count) (CInt win) (CInt surpWin) (CDouble thresh) (CDouble lamHi) (CDouble lamLo) (CDouble sRef) (CDouble k) out
  | count <= 0 || win <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let mp = MarkovParams (fromIntegral win) (fromIntegral surpWin) thresh lamHi lamLo sRef k
          vals = markovSeries mp (map (\(CDouble d) -> d) closes)
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

simGenerate :: CString -> CString -> IO CInt
simGenerate paramsPtr outPtr = do
  paramsStr <- peekCString paramsPtr
  outPath   <- peekCString outPtr
  case applyArgs (toArgs paramsStr) of
    Left _  -> pure 1
    Right p -> do
      writeIORef progressRef (0, spCandles p + 1)
      writeIORef cancelRef False
      result <- try (writeCsv outPath p) :: IO (Either SomeException ())
      case result of
        Left err
          | Just SimCancelled <- fromException err -> do
              cleanup outPath p
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
          readSidecarSize (ticksPath outPath)
          readLevelsSize (levelsPath outPath)
          readVevSize (vevPath outPath)
          result <- try (appendCsv outPath p n gs) :: IO (Either SomeException (GenState, Int))
          case result of
            Left err
              | Just SimCancelled <- fromException err -> do
                  restoreSidecar outPath p
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

writeCsv :: FilePath -> SimParams -> IO ()
writeCsv path p = do
  write (path ++ ".meta") (metaText p)
  if spSinus p
    then do
      -- sinus output is plain candles: drop stale tick/levels/vev sidecars
      rm (ticksPath path)
      rm (levelsPath path)
      rm (vevPath path)
      cs <- runSimSinusH simHooks p 0
      write path (csvText p cs)
      writeState (statePath path) p (length cs) (initState p)
    else case spMode p of
      ModeCandles -> do
        -- a candles-mode run leaves no sidecar: drop stale ones from an
        -- earlier same-name tick-mode run, or the UI would load the
        -- old ticks instead
        rm (ticksPath path)
        rm (levelsPath path)
        rm (vevPath path)
        (gs, cs) <- runSimH simHooks p (initState p)
        write path (csvText p cs)
        writeState (statePath path) p (length cs) gs
      _           -> do
        (gs, cs) <- withFile (ticksPath path) WriteMode $ \h -> do
          hSetBuffering h (BlockBuffering (Just (1024 * 1024)))
          hPutStr h (tickHeader (spBookDepth p) ++ "\n")
          withFile (levelsPath path) WriteMode $ \lh -> do
            hSetBuffering lh (BlockBuffering (Just (1024 * 1024)))
            hPutStr lh (lvlHeader ++ "\n")
            withVev (vevPath path) True $ \vh _ -> do
              (gs', cs', nEv) <- runSimTicksH simHooks (emitTick h) (emitLvlSeg (spPriceScale p) lh vh) p 0 (initState p) tr0
              patchVevCount vh (fromIntegral nEv)
              pure (gs', cs')
        write path (csvText p cs)
        writeState (statePath path) p (length cs) gs
  where
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)
    write fp s = withFile fp WriteMode $ \h -> hPutStr h s
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
    -- the seed book (start bid/ask) opens its level lifetimes at 0
    tr0 = trFromBook 0 0 (gsBook (initState p))
    simHooks = SimHooks
      { hookProgress = \d t -> writeIORef progressRef (d, t)
      , hookCancelled = readIORef cancelRef
      }

-- resume from a snapshot: generate more candles with the snapshot's
-- parameters, append the candles rows to the csv (creating it with a
-- header when missing), and stream the tick rows onto the sidecar
-- (creating it with a header when missing). The state snapshot is NOT
-- written here -- only after both files are complete. Returns the final
-- state and how many candles were produced.
appendCsv :: FilePath -> SimParams -> Int -> GenState -> IO (GenState, Int)
appendCsv path p done gs0 = do
  if spSinus p
    then do
      rm (ticksPath path)
      rm (levelsPath path)
      rm (vevPath path)
      cs <- runSimSinusH simHooks p done
      appendCandles path p cs
      pure (gs0, length cs)
    else case spMode p of
      ModeCandles -> do
        rm (ticksPath path)
        rm (levelsPath path)
        rm (vevPath path)
        (gs, cs) <- runSimH simHooks p gs0
        appendCandles path p cs
        pure (gs, length cs)
      _           -> do
        exists <- doesFileExist sidecar
        lexists <- doesFileExist lvls
        (gs, cs) <- withFile sidecar AppendMode $ \h -> do
          hSetBuffering h (BlockBuffering (Just (1024 * 1024)))
          unless exists $ hPutStr h (tickHeader (spBookDepth p) ++ "\n")
          withFile lvls AppendMode $ \lh -> do
            hSetBuffering lh (BlockBuffering (Just (1024 * 1024)))
            unless lexists $ hPutStr lh (lvlHeader ++ "\n")
            withVev (vevPath path) False $ \vh evBase -> do
              let tr0 = trFromBook (fromIntegral evBase) done (gsBook gs0)
              (gs', cs', nEv) <- runSimTicksH simHooks (emitTick h) (emitLvlSeg (spPriceScale p) lh vh) p done gs0 tr0
              patchVevCount vh (fromIntegral nEv)
              pure (gs', cs')
        appendCandles path p cs
        pure (gs, length cs)
  where
    sidecar = ticksPath path
    lvls = levelsPath path
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
    simHooks = SimHooks
      { hookProgress = \d t -> writeIORef progressRef (d, t)
      , hookCancelled = readIORef cancelRef
      }

-- the level sink: the csv row plus the lifetime's contiguous
-- volume-change events into the binary sidecar
emitLvlSeg :: Int -> Handle -> Handle -> (LvlSeg, [LvlEv]) -> IO ()
emitLvlSeg scale lh vh (seg, evs) = do
  hPutBuilder lh (lvlRowB scale seg <> char8 '\n')
  mapM_ (hPutBuilder vh . vevRecB) evs

patchVevCount :: Handle -> Word64 -> IO ()
patchVevCount vh n = do
  hSeek vh AbsoluteSeek 8
  hPutBuilder vh (word64LE n)

-- open the .vev sidecar: fresh = write the header and start counting at
-- 0; append = read the record count and seek to the end. Returns the
-- handle and the starting record count (the tracker continues its event
-- indexing from it)
withVev :: FilePath -> Bool -> (Handle -> Word64 -> IO a) -> IO a
withVev path fresh io = do
  exists <- doesFileExist path
  if fresh || not exists
    then withFile path WriteMode $ \vh -> do
      hSetBuffering vh (BlockBuffering (Just (1024 * 1024)))
      hPutBuilder vh vevHeader
      io vh 0
    else withFile path ReadWriteMode $ \vh -> do
      hSetBuffering vh (BlockBuffering (Just (1024 * 1024)))
      hSeek vh AbsoluteSeek 8
      cntBytes <- B.hGet vh 8
      hSeek vh SeekFromEnd 0
      io vh (decodeLE64 cntBytes)

decodeLE64 :: B.ByteString -> Word64
decodeLE64 = B.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 . B.reverse

appendCandles :: FilePath -> SimParams -> [Candle] -> IO ()
appendCandles path p cs = do
  exists <- doesFileExist path
  withFile path AppendMode $ \h ->
    hPutStr h $ (if exists then "" else csvHeader) ++ csvRowsText p cs
  where
    csvHeader = "open,high,low,close,vol\n"

-- the sidecar sizes before a resume run, recorded by restoreSidecar's
-- caller via readSidecarSize/readLevelsSize
sidecarSizeRef :: IORef (Maybe Integer)
sidecarSizeRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE sidecarSizeRef #-}

levelsSizeRef :: IORef (Maybe Integer)
levelsSizeRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE levelsSizeRef #-}

vevSizeRef :: IORef (Maybe Integer)
vevSizeRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE vevSizeRef #-}

readSidecarSize :: FilePath -> IO ()
readSidecarSize path = do
  exists <- doesFileExist path
  sz <- if exists then Just <$> getFileSize path else pure Nothing
  writeIORef sidecarSizeRef sz

readLevelsSize :: FilePath -> IO ()
readLevelsSize path = do
  exists <- doesFileExist path
  sz <- if exists then Just <$> getFileSize path else pure Nothing
  writeIORef levelsSizeRef sz

readVevSize :: FilePath -> IO ()
readVevSize path = do
  exists <- doesFileExist path
  sz <- if exists then Just <$> getFileSize path else pure Nothing
  writeIORef vevSizeRef sz

-- undo partially streamed sidecars after a cancellation: truncate all
-- three back to the recorded sizes (or remove a file that did not exist
-- before). The .vev count header is not patched on cancel, but the
-- truncation removes the partial records so the count stays consistent
restoreSidecar :: FilePath -> SimParams -> IO ()
restoreSidecar path p
  | spSinus p = pure ()
  | otherwise = case spMode p of
      ModeCandles -> pure ()
      _           -> do
        sz <- readIORef sidecarSizeRef
        case sz of
          Nothing -> rm sidecar
          Just n  -> withFile sidecar AppendMode (`hSetFileSize` n)
        lsz <- readIORef levelsSizeRef
        case lsz of
          Nothing -> rm lvls
          Just n  -> withFile lvls AppendMode (`hSetFileSize` n)
        vsz <- readIORef vevSizeRef
        case vsz of
          Nothing -> rm vevp
          Just n  -> withFile vevp AppendMode (`hSetFileSize` n)
  where
    sidecar = ticksPath path
    lvls = levelsPath path
    vevp = vevPath path
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)

-- remove partial output after a cancellation (missing files ignored)
cleanup :: FilePath -> SimParams -> IO ()
cleanup path p = mapM_ rm (path : (path ++ ".meta") : sidecar)
  where
    sidecar = case spMode p of
      ModeCandles -> []
      _           -> [ticksPath path | not (spSinus p)] ++ [levelsPath path | not (spSinus p)] ++ [vevPath path | not (spSinus p)]
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)

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
      result <- try (runDslBacktest p) :: IO (Either SomeException ())
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

runDslBacktest :: DslParams -> IO ()
runDslBacktest p = do
  parsed <- MLD.loadBars (dspData p)
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  let closes = VU.fromList (map MLD.barC bars)
      vols = VU.fromList (map MLD.barV bars)
      n = VU.length closes
      warmup = STD.dslMaxPeriod (dspSpec p) - 1
  when (n < warmup + 12) (ioError (userError "not enough candles for the strategy warm-up"))
  createDirectoryIfMissing True (dspOut p)
  let (sig, causes) = STD.dslSignalCause (dspSpec p) closes
      (bt, trades) = STB.backtestWithTrades (dspFee p) closes vols sig causes (warmup, n - 1)
      met = STB.brMetrics bt
      eqPath = dspOut p ++ "/equity_0.csv"
      specText = STD.dslText (dspSpec p)
      js = intercalate ","
        [ "\"seed\":0"
        , "\"slots\":" ++ jsonStr specText
        , "\"valCalmar\":" ++ show (STB.bmCalmar met)
        , "\"pnl\":" ++ show (STB.bmPnl met)
        , "\"maxDD\":" ++ show (STB.bmMaxDD met)
        , "\"calmar\":" ++ show (STB.bmCalmar met)
        , "\"trades\":" ++ show (STB.bmTrades met)
        , "\"equity\":\"equity_0.csv\""
        , "\"cand\":\"cand_0.csv\""
        , "\"test\":\"test_0.csv\""
        ]
  withFile (dspOut p ++ "/strat_0.json") WriteMode $ \h ->
    hPutStr h ("{" ++ js ++ "}\n")
  withFile eqPath WriteMode $ \h -> do
    hPutStr h "open,high,low,close,vol\n"
    hPutStr h (concatMap eqRow (VU.toList (STB.brEquity bt)))
  withFile (dspOut p ++ "/cand_0.csv") WriteMode $ \h -> do
    hPutStr h "candidate,val_calmar\n"
    hPutStr h ("1," ++ printf "%.6g" (STB.bmCalmar met) ++ "\n")
  withFile (dspOut p ++ "/test_0.csv") WriteMode $ \h -> do
    hPutStr h "close,equity,pos\n"
    hPutStr h (concatMap testRow (zip3 (VU.toList (STB.brCloses bt))
                                       (VU.toList (STB.brEquity bt))
                                       (VU.toList (STB.brPos bt))))
  withFile (dspOut p ++ "/trades_0.bin") WriteMode $ \h ->
    hPutBuilder h (tradesBin trades)
  where
    eqRow e =
      let s = printf "%.5f" e
      in s ++ "," ++ s ++ "," ++ s ++ "," ++ s ++ ",0\n"
    testRow (c, e, po) = printf "%.5f,%.6g,%d\n" c e po

-- minimal JSON string escaping (DSL specs contain newlines)
jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '\n' = "\\n"
    esc c | c == '"' || c == '\\' = ['\\', c]
          | otherwise = [c]

runStratBatch :: StratParams -> IO ()
runStratBatch p = do
  parsed <- MLD.loadBars (stpData p)
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  let closes = VU.fromList (map MLD.barC bars)
      vols = VU.fromList (map MLD.barV bars)
  case STO.optSplits (stpOpt p) (VU.length closes) of
    Left err -> ioError (userError err)
    Right _  -> pure ()
  createDirectoryIfMissing True (stpOut p)
  let cands = max 1 (STO.opCandidates (stpOpt p))
      n = max 1 (stpSessions p)
  stRefs <- forM [0 .. n - 1] $ \k -> do
    c <- newIORef 0
    b <- newIORef (-1 / 0)
    s <- newIORef "run"
    pure (StratSessionState (stpSeedBase p + fromIntegral k) c cands b s)
  writeIORef stratSessionsRef stRefs
  writeIORef stratProgressRef (0, n * cands)
  doneRef <- newIORef 0
  barrier <- newEmptyMVar
  _ <- forM stRefs $ \st -> forkIO $ do
    r <- try (STO.optimizeSession (stratHooks st) (stpOpt p) closes vols (ssSeed st))
    case r of
      Left err -> writeIORef (ssStatus st)
        (case fromException err of
           Just SimCancelled -> "cancelled"
           _                 -> "error")
      Right res -> do
        writeIORef (ssStatus st) "done"
        writeStratResult (stpOut p) res
    finished <- atomicModifyIORef' doneRef (\d -> (d + 1, d + 1))
    when (finished == n) (putMVar barrier ())
  takeMVar barrier
  pure ()
  where
    stratHooks st = STO.OptHooks
      { STO.ohProgress = \c _ -> do
          writeIORef (ssCand st) c
          _ <- atomicModifyIORef' stratProgressRef (\(d, t) -> ((d + 1, t), ()))
          pure ()
      , STO.ohBest = \cm -> do
          best <- readIORef (ssBest st)
          when (cm > best) (writeIORef (ssBest st) cm)
      , STO.ohCancelled = readIORef stratCancelRef
      }

-- per-session artifacts: metrics json (the winning spec + test metrics),
-- the test equity curve as a candles csv, the val-Calmar-per-candidate
-- history and the per-test-step series (close, equity, position) for the
-- strategy page's plot widgets
writeStratResult :: FilePath -> STO.OptResult -> IO ()
writeStratResult out r = do
  let bt = STO.orTest r
      met = STB.brMetrics bt
      seed = STO.orSeed r
      spec = STR.stratText (STO.orStrat r)
      eqPath = out ++ "/equity_" ++ show seed ++ ".csv"
      js = intercalate ","
        [ "\"seed\":" ++ show seed
        , "\"slots\":\"" ++ spec ++ "\""
        , "\"valCalmar\":" ++ show (STO.orValCalmar r)
        , "\"pnl\":" ++ show (STB.bmPnl met)
        , "\"maxDD\":" ++ show (STB.bmMaxDD met)
        , "\"calmar\":" ++ show (STB.bmCalmar met)
        , "\"trades\":" ++ show (STB.bmTrades met)
        , "\"equity\":\"equity_" ++ show seed ++ ".csv\""
        , "\"cand\":\"cand_" ++ show seed ++ ".csv\""
        , "\"test\":\"test_" ++ show seed ++ ".csv\""
        ]
  withFile (out ++ "/strat_" ++ show seed ++ ".json") WriteMode $ \h ->
    hPutStr h ("{" ++ js ++ "}\n")
  withFile eqPath WriteMode $ \h -> do
    hPutStr h "open,high,low,close,vol\n"
    hPutStr h (concatMap eqRow (VU.toList (STB.brEquity bt)))
  withFile (out ++ "/cand_" ++ show seed ++ ".csv") WriteMode $ \h -> do
    hPutStr h "candidate,val_calmar\n"
    hPutStr h (concatMap candRow (zip [(1 :: Int) ..] (VU.toList (STO.orCandCalmar r))))
  withFile (out ++ "/test_" ++ show seed ++ ".csv") WriteMode $ \h -> do
    hPutStr h "close,equity,pos\n"
    hPutStr h (concatMap testRow (zip3 (VU.toList (STB.brCloses bt))
                                       (VU.toList (STB.brEquity bt))
                                       (VU.toList (STB.brPos bt))))
  withFile (out ++ "/trades_" ++ show seed ++ ".bin") WriteMode $ \h ->
    hPutBuilder h (tradesBin (STO.orTrades r))
  where
    eqRow e =
      let s = printf "%.5f" e
      in s ++ "," ++ s ++ "," ++ s ++ "," ++ s ++ ",0\n"
    candRow (c, v) = show c ++ "," ++ printf "%.6g" v ++ "\n"
    testRow (c, e, p) = printf "%.5f,%.6g,%d\n" c e p

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
  parsed <- MLD.loadBars (mlpData p)
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  md <- case MLT.prepareData (mlpTrain p) bars of
    Left err -> ioError (userError err)
    Right d  -> pure d
  createDirectoryIfMissing True (mlpOut p)
  let epochs = MLT.tpEpochs (mlpTrain p)
      n = max 1 (mlpSessions p)
  stRefs <- forM [0 .. n - 1] $ \k -> do
    e <- newIORef 0
    b <- newIORef (-1 / 0)
    s <- newIORef "run"
    pure (MlSessionState (mlpSeedBase p + fromIntegral k) e epochs b s)
  writeIORef mlSessionsRef stRefs
  writeIORef mlProgressRef (0, n * epochs)
  doneRef <- newIORef 0
  barrier <- newEmptyMVar
  _ <- forM stRefs $ \st -> forkIO $ do
    r <- try (MLT.trainSession (mlHooks st) (mlpTrain p) md (msSeed st))
    case r of
      Left err -> writeIORef (msStatus st)
        (case fromException err of
           Just SimCancelled -> "cancelled"
           _                 -> "error")
      Right Nothing -> writeIORef (msStatus st) "cancelled"
      Right (Just sr) -> do
        writeIORef (msStatus st) "done"
        writeMlResult (mlpOut p) sr
    finished <- atomicModifyIORef' doneRef (\d -> (d + 1, d + 1))
    when (finished == n) (putMVar barrier ())
  takeMVar barrier
  pure ()
  where
    mlHooks st = MLT.TrainHooks
      { MLT.thProgress = \e _ -> do
          writeIORef (msEpoch st) e
          _ <- atomicModifyIORef' mlProgressRef (\(d, t) -> ((d + 1, t), ()))
          pure ()
      , MLT.thBest = \vc -> do
          best <- readIORef (msBest st)
          when (vc > best) (writeIORef (msBest st) vc)
      , MLT.thCancelled = readIORef mlCancelRef
      }

-- per-session artifacts: metrics json, the equity curve as a candles csv
-- (o=h=l=c=equity) so the chart can load it as a regular timeline, the
-- validation-Calmar-per-epoch history, and the per-test-step series
-- (close, equity, predicted class) for the ML page's own plot widgets
writeMlResult :: FilePath -> MLT.SessionResult -> IO ()
writeMlResult out sr = do
  let pr = MLT.srTest sr
      met = MLT.prMetrics pr
      seed = MLT.srSeed sr
      eqPath = out ++ "/equity_" ++ show seed ++ ".csv"
      js = intercalate ","
        [ "\"seed\":" ++ show seed
        , "\"epochs\":" ++ show (MLT.srEpochs sr)
        , "\"bestEpoch\":" ++ show (MLT.srBestEpoch sr)
        , "\"valCalmar\":" ++ show (if VU.null (MLT.srHistory sr) then 0 else VU.maximum (MLT.srHistory sr))
        , "\"pnl\":" ++ show (MLT.mPnl met)
        , "\"maxDD\":" ++ show (MLT.mMaxDD met)
        , "\"calmar\":" ++ show (MLT.mCalmar met)
        , "\"accuracy\":" ++ show (MLT.mAccuracy met)
        , "\"trades\":" ++ show (MLT.mTrades met)
        , "\"equity\":\"equity_" ++ show seed ++ ".csv\""
        , "\"hist\":\"hist_" ++ show seed ++ ".csv\""
        , "\"test\":\"test_" ++ show seed ++ ".csv\""
        ]
  withFile (out ++ "/ml_" ++ show seed ++ ".json") WriteMode $ \h ->
    hPutStr h ("{" ++ js ++ "}\n")
  withFile eqPath WriteMode $ \h -> do
    hPutStr h "open,high,low,close,vol\n"
    hPutStr h (concatMap eqRow (VU.toList (MLT.prEquity pr)))
  withFile (out ++ "/hist_" ++ show seed ++ ".csv") WriteMode $ \h -> do
    hPutStr h "epoch,val_calmar\n"
    hPutStr h (concatMap histRow (zip [(1 :: Int) ..] (VU.toList (MLT.srHistory sr))))
  withFile (out ++ "/test_" ++ show seed ++ ".csv") WriteMode $ \h -> do
    hPutStr h "close,equity,pred\n"
    hPutStr h (concatMap testRow (zip3 (VU.toList (MLT.prCloses pr))
                                       (VU.toList (MLT.prEquity pr))
                                       (VU.toList (MLT.prPreds pr))))
  where
    eqRow e =
      let s = printf "%.5f" e
      in s ++ "," ++ s ++ "," ++ s ++ "," ++ s ++ ",0\n"
    histRow (e, v) = show e ++ "," ++ printf "%.6g" v ++ "\n"
    testRow (c, e, p) = printf "%.5f,%.6g,%d\n" c e p

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
    readK k d = maybe (Right d) (readTK k) (lookup k kv)
    readKD :: String -> Double -> Either String Double
    readKD k d = maybe (Right d) (readTK k) (lookup k kv)
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
