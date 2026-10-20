{-# LANGUAGE StrictData #-}

-- shared run/batch orchestration for the FFI (SimFFI) and the MCP server
-- (Sim.Mcp.Tools): CSV/sidecar generation with resumable state, ML
-- training batches and strategy optimization batches. Long-running
-- entry points take hooks/progress callbacks as parameters — the FFI
-- wires them to its polled global refs, the MCP server wires them to
-- JSON-RPC progress notifications.
module Sim.Run
  ( -- generation + resume
    generateCsv
  , appendCsvRes
  , cleanupOutput
  , SidecarSizes
  , newSidecarSizes
  , recordSidecarSizes
  , restoreSidecars
    -- ML trainer
  , runMlBatch
  , writeMlResult
    -- DTW correlation scan
  , DtwPlan (..)
  , DtwHooks (..)
  , runDtwBatch
  , writeDtwResult
    -- strategy optimizer + DSL
  , runStratBatch
  , runDslBacktest
  , writeStratResult
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, fromException, ioError, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as B
import Data.ByteString.Builder (char8, hPutBuilder, word64LE)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Sim.Csv (csvRowsText, csvText, lvlHeader, lvlRowB, levelsPath, metaText, tickHeader, tickRowB, ticksPath, tradesBin, vevHeader, vevPath, vevRecB)
import Sim.Dtw (mkPrefix)
import Sim.DtwIndex (ScanParams (..), ScanResult (..))
import qualified Sim.DtwIndex as DTW
import Sim.Gen (GenState (..), initState)
import Sim.GenH (runSimH)
import Sim.Lvl (LvlEv, LvlSeg, trFromBook)
import qualified Sim.ML.Data as MLD
import qualified Sim.ML.Train as MLT
import Sim.Resume (statePath, writeState)
import Sim.Sinus (runSimSinusH)
import qualified Sim.Strat.Backtest as STB
import qualified Sim.Strat.Dsl as STD
import qualified Sim.Strat.Optimize as STO
import qualified Sim.Strat.Rules as STR
import Sim.TickGenH (runSimTicksH)
import Sim.Types
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (AppendMode, ReadWriteMode, WriteMode), SeekMode (AbsoluteSeek, SeekFromEnd), hPutStr, hSeek, hSetBuffering, hSetFileSize, withFile)
import System.IO.Error (isDoesNotExistError, userError)
import Text.Printf (printf)

-- --- generation -------------------------------------------------------------

-- write a full run next to `path`: candles csv + .meta + <basename>.state,
-- streaming tick/levels/vev sidecars in tick modes (stale sidecars from
-- an earlier same-name run are removed in candles/sinus modes)
generateCsv :: SimHooks -> FilePath -> SimParams -> IO ()
generateCsv simHooks path p = do
  write (path ++ ".meta") (metaText p)
  if spSinus p
    then do
      rm (ticksPath path)
      rm (levelsPath path)
      rm (vevPath path)
      cs <- runSimSinusH simHooks p 0
      write path (csvText p cs)
      writeState (statePath path) p (length cs) (initState p)
    else case spMode p of
      ModeCandles -> do
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
    rm fp = removeFile fp `catch'` \e -> unless (isDoesNotExistError e) (ioError e)
    write fp s = withFile fp WriteMode $ \h -> hPutStr h s
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
    -- the seed book (start bid/ask) opens its level lifetimes at 0
    tr0 = trFromBook 0 0 (gsBook (initState p))

-- resume from a snapshot: generate more candles with the snapshot's
-- parameters, append the candles rows to the csv (creating it with a
-- header when missing), and stream the tick rows onto the sidecar
-- (creating it with a header when missing). The state snapshot is NOT
-- written here -- only after both files are complete. Returns the final
-- state and how many candles were produced.
appendCsvRes :: SimHooks -> FilePath -> SimParams -> Int -> GenState -> IO (GenState, Int)
appendCsvRes simHooks path p done gs0 = do
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
    rm fp = removeFile fp `catch'` \e -> unless (isDoesNotExistError e) (ioError e)
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')

-- remove partial output after a cancellation (missing files ignored)
cleanupOutput :: FilePath -> SimParams -> IO ()
cleanupOutput path p = mapM_ rm (path : (path ++ ".meta") : sidecar)
  where
    sidecar = case spMode p of
      ModeCandles -> []
      _           -> [ticksPath path | not (spSinus p)] ++ [levelsPath path | not (spSinus p)] ++ [vevPath path | not (spSinus p)]
    rm fp = removeFile fp `catch'` \e -> unless (isDoesNotExistError e) (ioError e)

appendCandles :: FilePath -> SimParams -> [Candle] -> IO ()
appendCandles path p cs = do
  exists <- doesFileExist path
  withFile path AppendMode $ \h ->
    hPutStr h $ (if exists then "" else csvHeader) ++ csvRowsText p cs
  where
    csvHeader = "open,high,low,close,vol\n"

-- --- sidecar size snapshot for cancellation restore -------------------------

data SidecarSizes = SidecarSizes
  { ssTicks  :: !(IORef (Maybe Integer))
  , ssLevels :: !(IORef (Maybe Integer))
  , ssVev    :: !(IORef (Maybe Integer))
  }

newSidecarSizes :: IO SidecarSizes
newSidecarSizes = SidecarSizes <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing

recordSidecarSizes :: FilePath -> SidecarSizes -> IO ()
recordSidecarSizes path ss = do
  record (ticksPath path) ssTicks
  record (levelsPath path) ssLevels
  record (vevPath path) ssVev
  where
    record fp field = do
      exists <- doesFileExist fp
      sz <- if exists then Just <$> getFileSize fp else pure Nothing
      writeIORef (field ss) sz

-- undo partially streamed sidecars after a cancellation: truncate all
-- three back to the recorded sizes (or remove a file that did not exist
-- before). The .vev count header is not patched on cancel, but the
-- truncation removes the partial records so the count stays consistent
restoreSidecars :: FilePath -> SimParams -> SidecarSizes -> IO ()
restoreSidecars path p ss
  | spSinus p = pure ()
  | otherwise = case spMode p of
      ModeCandles -> pure ()
      _           -> do
        sz <- readIORef (ssTicks ss)
        case sz of
          Nothing -> rm sidecar
          Just n  -> withFile sidecar AppendMode (`hSetFileSize` n)
        lsz <- readIORef (ssLevels ss)
        case lsz of
          Nothing -> rm lvls
          Just n  -> withFile lvls AppendMode (`hSetFileSize` n)
        vsz <- readIORef (ssVev ss)
        case vsz of
          Nothing -> rm vevp
          Just n  -> withFile vevp AppendMode (`hSetFileSize` n)
  where
    sidecar = ticksPath path
    lvls = levelsPath path
    vevp = vevPath path
    rm fp = removeFile fp `catch'` \e -> unless (isDoesNotExistError e) (ioError e)

-- --- ML trainer -------------------------------------------------------------

-- run a batch of parallel training sessions on one dataset; each session
-- gets its own hooks (seed, progress, best-so-far, cancel). Blocks until
-- all sessions finish. Artifacts per session: <out>/ml_<seed>.json,
-- equity_<seed>.csv, hist_<seed>.csv, test_<seed>.csv.
runMlBatch :: MLT.TrainParams -> FilePath -> FilePath -> [(Word64, MLT.TrainHooks)] -> IO ()
runMlBatch tp dataPath out sessions = do
  parsed <- MLD.loadBars dataPath
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  md <- case MLT.prepareData tp bars of
    Left err -> ioError (userError err)
    Right d  -> pure d
  createDirectoryIfMissing True out
  doneRef <- newIORef 0
  barrier <- newEmptyMVar
  let n = length sessions
  _ <- forM sessions $ \(seed, hooks) -> forkIO $ do
    r <- try (MLT.trainSession hooks tp md seed) :: IO (Either SomeException (Maybe MLT.SessionResult))
    case r of
      Right (Just sr) -> writeMlResult out sr
      _               -> pure ()
    finished <- atomicModifyIORef' doneRef (\d -> (d + 1, d + 1))
    when (finished == n) (putMVar barrier ())
  takeMVar barrier

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

-- --- DTW correlation scan ---------------------------------------------------

-- one scan session: series A (tfA) windows against series B (tfB) at one
-- window length. Artifacts per session: <out>/dtw_<tfA>_<tfB>_<win>.json
-- (summary) and .csv (the position line: one row per stepped A window).
data DtwPlan = DtwPlan
  { dpTfA      :: !String
  , dpTfB      :: !String
  , dpWin      :: !Int
  , dpBand     :: !Int
  , dpPaa      :: !Int
  , dpCard     :: !Int
  , dpStep     :: !Int
  , dpMaxCand  :: !Int
  } deriving (Show)

data DtwHooks = DtwHooks
  { dwProgress  :: Int -> Int -> IO ()
  , dwBest      :: Double -> IO ()
  , dwCancelled :: IO Bool
  }

-- run all plans in parallel (one forkIO each); blocks until every session
-- finishes (cancelled sessions return Nothing and write no artifacts).
-- Each plan carries its own A/B closes vectors so one batch can scan a
-- whole timeframe-permutation grid.
runDtwBatch :: FilePath -> [(DtwPlan, VU.Vector Double, VU.Vector Double, DtwHooks)] -> IO ()
runDtwBatch out jobs = do
  createDirectoryIfMissing True out
  doneRef <- newIORef 0
  barrier <- newEmptyMVar
  let n = length jobs
  forM jobs $ \(pl, closesA, closesB, hooks) -> forkIO $ do
    r <- try (runDtwSession hooks (planScanParams pl) closesA closesB) :: IO (Either SomeException (Maybe ScanResult))
    case r of
      Right (Just res) -> writeDtwResult out pl res
      _                -> pure ()
    finished <- atomicModifyIORef' doneRef (\d -> (d + 1, d + 1))
    when (finished == n) (putMVar barrier ())
  takeMVar barrier

planScanParams :: DtwPlan -> ScanParams
planScanParams pl = ScanParams (dpWin pl) (dpBand pl) (dpPaa pl) (dpCard pl) (dpStep pl) (dpMaxCand pl)

-- hooked scan: one matchAt per stepped A window, progress + cancel polled
-- every 16 windows. Nothing when cancelled or params/series invalid.
runDtwSession :: DtwHooks -> ScanParams -> VU.Vector Double -> VU.Vector Double -> IO (Maybe ScanResult)
runDtwSession hooks sp a b
  | not (DTW.scanValid sp (VU.length a) (VU.length b)) = pure Nothing
  | otherwise = do
      let pfa = mkPrefix a
          ctx = DTW.buildCtx sp b
          wins = [0, scStep sp .. VU.length a - scWin sp]
          total = length wins
      go pfa ctx wins total 0 [] (1 / 0)
  where
  go _ _ [] total _ acc bestD = do
    dwProgress hooks total total
    dwBest hooks bestD
    let matches = VU.fromList (reverse acc)
        bestEntry = case [m | m@(_, j, _) <- acc, j >= 0] of
          [] -> Nothing
          m0 : ms -> Just (foldl' (\e@(_, _, da) m@(_, _, d') -> if d' < da then m else e) m0 ms)
    pure (Just (ScanResult matches bestEntry))
  go pfa ctx (i : rest) total done acc bestD = do
    cancelled <- if done `mod` 16 == 0 then dwCancelled hooks else pure False
    if cancelled
      then throwIO SimCancelled
      else do
        let (i', j, d) = DTW.matchAt sp ctx pfa i
            bestD' = if d == d && d < bestD then d else bestD
        dwProgress hooks (done + 1) total
        when (done `mod` 64 == 0) (dwBest hooks bestD')
        go pfa ctx rest total (done + 1) ((i', j, d) : acc) bestD'

-- per-session artifacts: summary json + the position-line csv
writeDtwResult :: FilePath -> DtwPlan -> ScanResult -> IO ()
writeDtwResult out pl res = do
  let base = "dtw_" ++ dpTfA pl ++ "_" ++ dpTfB pl ++ "_" ++ show (dpWin pl)
      entries = [ (i, j, d) | (i, j, d) <- VU.toList (srMatches res), j >= 0, d == d ]
      found = length entries
      js = intercalate ","
        [ "\"tfA\":\"" ++ dpTfA pl ++ "\""
        , "\"tfB\":\"" ++ dpTfB pl ++ "\""
        , "\"win\":" ++ show (dpWin pl)
        , "\"band\":" ++ show (dpBand pl)
        , "\"windows\":" ++ show (VU.length (srMatches res))
        , "\"found\":" ++ show found
        , "\"best\":" ++ bestJson
        , "\"csv\":\"" ++ base ++ ".csv\""
        ]
      bestJson = case srBest res of
        Just (i, j, d) | d == d ->
          "{\"i\":" ++ show i ++ ",\"j\":" ++ show j ++ ",\"dist\":" ++ show d ++ "}"
        _ -> "null"
  withFile (out ++ "/" ++ base ++ ".json") WriteMode $ \h ->
    hPutStr h ("{" ++ js ++ "}\n")
  withFile (out ++ "/" ++ base ++ ".csv") WriteMode $ \h -> do
    hPutStr h "i,j,dist\n"
    forM_ entries $ \(i, j, d) -> hPutStr h (show i ++ "," ++ show j ++ "," ++ show d ++ "\n")

-- --- strategy optimizer -----------------------------------------------------

-- run a batch of parallel optimization sessions; each session gets its
-- own hooks. Blocks until all sessions finish. Artifacts per session:
-- <out>/strat_<seed>.json, equity_<seed>.csv, cand_<seed>.csv,
-- test_<seed>.csv, trades_<seed>.bin.
runStratBatch :: STO.OptParams -> FilePath -> FilePath -> [(Word64, STO.OptHooks)] -> IO ()
runStratBatch op dataPath out sessions = do
  parsed <- MLD.loadBars dataPath
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  let closes = VU.fromList (map MLD.barC bars)
      vols = VU.fromList (map MLD.barV bars)
  case STO.optSplits op (VU.length closes) of
    Left err -> ioError (userError err)
    Right _  -> pure ()
  createDirectoryIfMissing True out
  doneRef <- newIORef 0
  barrier <- newEmptyMVar
  let n = length sessions
  _ <- forM sessions $ \(seed, hooks) -> forkIO $ do
    r <- try (STO.optimizeSession hooks op closes vols seed) :: IO (Either SomeException STO.OptResult)
    case r of
      Right res -> writeStratResult out res
      Left _    -> pure ()
    finished <- atomicModifyIORef' doneRef (\d -> (d + 1, d + 1))
    when (finished == n) (putMVar barrier ())
  takeMVar barrier

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

-- backtest one hand-authored DSL strategy (Sim.Strat.Dsl) over a dataset
-- and write the result in the SAME artifact layout the optimizer uses,
-- with seed 0: <out>/strat_0.json, equity_0.csv, cand_0.csv (a single
-- candidate row), test_0.csv, trades_0.bin
runDslBacktest :: FilePath -> FilePath -> STD.DslStrat -> Double -> IO ()
runDslBacktest dataPath out spec fee = do
  parsed <- MLD.loadBars dataPath
  bars <- case parsed of
    Left err -> ioError (userError err)
    Right bs -> pure bs
  let closes = VU.fromList (map MLD.barC bars)
      vols = VU.fromList (map MLD.barV bars)
      n = VU.length closes
      warmup = STD.dslMaxPeriod spec - 1
  when (n < warmup + 12) (ioError (userError "not enough candles for the strategy warm-up"))
  createDirectoryIfMissing True out
  let (sig, causes) = STD.dslSignalCause spec closes
      (bt, trades) = STB.backtestWithTrades fee closes vols sig causes (warmup, n - 1)
      met = STB.brMetrics bt
      eqPath = out ++ "/equity_0.csv"
      specText = STD.dslText spec
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
  withFile (out ++ "/strat_0.json") WriteMode $ \h ->
    hPutStr h ("{" ++ js ++ "}\n")
  withFile eqPath WriteMode $ \h -> do
    hPutStr h "open,high,low,close,vol\n"
    hPutStr h (concatMap eqRow (VU.toList (STB.brEquity bt)))
  withFile (out ++ "/cand_0.csv") WriteMode $ \h -> do
    hPutStr h "candidate,val_calmar\n"
    hPutStr h ("1," ++ printf "%.6g" (STB.bmCalmar met) ++ "\n")
  withFile (out ++ "/test_0.csv") WriteMode $ \h -> do
    hPutStr h "close,equity,pos\n"
    hPutStr h (concatMap testRow (zip3 (VU.toList (STB.brCloses bt))
                                       (VU.toList (STB.brEquity bt))
                                       (VU.toList (STB.brPos bt))))
  withFile (out ++ "/trades_0.bin") WriteMode $ \h ->
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

-- --- level/vev plumbing (moved from SimFFI) ---------------------------------

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

catch' :: IO a -> (IOError -> IO a) -> IO a
catch' = catch
