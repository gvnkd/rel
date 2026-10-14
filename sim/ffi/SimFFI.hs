module SimFFI where

import Control.Exception (SomeException, catch, fromException, ioError, try)
import Control.Monad (unless)
import Data.ByteString.Builder (char8, hPutBuilder)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Sim.Cli (applyArgs)
import Sim.Csv (csvRowsText, csvText, metaText, tickHeader, tickRowB, ticksPath)
import Sim.Entropy (rollingPermEntropy)
import Sim.Fft (fftApprox, fftBest)
import Sim.Gen (GenState, initState)
import Sim.GenH (runSimH)
import Sim.Resume (readState, statePath, writeState)
import Sim.TickGenH (runSimTicksH)
import Sim.Types (Candle, SimHooks (..), SimMode (..), SimParams (..), SimCancelled (..))
import System.Directory (doesFileExist, getFileSize, removeFile)
import System.IO (BufferMode (BlockBuffering), IOMode (AppendMode, WriteMode), hPutStr, hSetBuffering, hSetFileSize, withFile)
import System.IO.Error (isDoesNotExistError)

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
-- forecast — into out, returns the total count (0 on invalid input)
foreign export ccall "sim_fft_forecast" simFftForecast
  :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt

simFftForecast :: Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt
simFftForecast ptr (CInt count) (CInt start) (CInt end) (CInt predict) (CInt waves) out
  | count <= 0 || predict <= 0 = pure 0
  | otherwise = do
      closes <- peekArray (fromIntegral count) ptr
      let (fit, fc) = fftApprox (fromIntegral waves) (map (\(CDouble d) -> d) closes) (fromIntegral start) (fromIntegral end) (fromIntegral predict)
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
  case spMode p of
    ModeCandles -> do
      -- a candles-mode run leaves no sidecar: drop a stale one from an
      -- earlier same-name tick-mode run, or the UI would load it instead
      rm (ticksPath path)
      (gs, cs) <- runSimH simHooks p (initState p)
      write path (csvText p cs)
      writeState (statePath path) p (length cs) gs
    _           -> do
      (gs, cs) <- withFile (ticksPath path) WriteMode $ \h -> do
        hSetBuffering h (BlockBuffering (Just (1024 * 1024)))
        hPutStr h (tickHeader (spBookDepth p) ++ "\n")
        runSimTicksH simHooks (emitTick h) p 0 (initState p)
      write path (csvText p cs)
      writeState (statePath path) p (length cs) gs
  where
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)
    write fp s = withFile fp WriteMode $ \h -> hPutStr h s
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
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
  case spMode p of
    ModeCandles -> do
      rm (ticksPath path)
      (gs, cs) <- runSimH simHooks p gs0
      appendCandles path p cs
      pure (gs, length cs)
    _           -> do
      exists <- doesFileExist sidecar
      (gs, cs) <- withFile sidecar AppendMode $ \h -> do
        hSetBuffering h (BlockBuffering (Just (1024 * 1024)))
        unless exists $ hPutStr h (tickHeader (spBookDepth p) ++ "\n")
        runSimTicksH simHooks (emitTick h) p done gs0
      appendCandles path p cs
      pure (gs, length cs)
  where
    sidecar = ticksPath path
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
    simHooks = SimHooks
      { hookProgress = \d t -> writeIORef progressRef (d, t)
      , hookCancelled = readIORef cancelRef
      }

appendCandles :: FilePath -> SimParams -> [Candle] -> IO ()
appendCandles path p cs = do
  exists <- doesFileExist path
  withFile path AppendMode $ \h ->
    hPutStr h $ (if exists then "" else csvHeader) ++ csvRowsText p cs
  where
    csvHeader = "open,high,low,close,vol\n"

-- the sidecar size before the run, recorded by restoreSidecar's caller
-- via readSidecarSize
sidecarSizeRef :: IORef (Maybe Integer)
sidecarSizeRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE sidecarSizeRef #-}

readSidecarSize :: FilePath -> IO ()
readSidecarSize path = do
  exists <- doesFileExist path
  sz <- if exists then Just <$> getFileSize path else pure Nothing
  writeIORef sidecarSizeRef sz

-- undo a partially streamed sidecar after a cancellation: truncate back
-- to the recorded size (or remove a file that did not exist before)
restoreSidecar :: FilePath -> SimParams -> IO ()
restoreSidecar path p = case spMode p of
  ModeCandles -> pure ()
  _           -> do
    sz <- readIORef sidecarSizeRef
    case sz of
      Nothing -> rm sidecar
      Just n  -> withFile sidecar AppendMode (`hSetFileSize` n)
  where
    sidecar = ticksPath path
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)

-- remove partial output after a cancellation (missing files ignored)
cleanup :: FilePath -> SimParams -> IO ()
cleanup path p = mapM_ rm (path : (path ++ ".meta") : sidecar)
  where
    sidecar = case spMode p of
      ModeCandles -> []
      _           -> [ticksPath path]
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)

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
