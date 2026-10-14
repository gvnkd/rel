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
import Sim.Csv (csvText, metaText, tickHeader, tickRowB, ticksPath)
import Sim.Fft (fftApprox)
import Sim.GenH (runSimH)
import Sim.TickGenH (runSimTicksH)
import Sim.Types (SimHooks (..), SimMode (..), SimParams (..), SimCancelled (..))
import System.Directory (removeFile)
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import System.IO.Error (isDoesNotExistError)

-- params are "key=value" pairs separated by ';', keys as in the CLI
-- without the leading "--" (e.g. "candles=1000;seed=7")
--
-- returns 0 on success, 1 on bad params, 2 on an IO/generation
-- failure, 3 when cancelled via sim_cancel (partial output removed)
foreign export ccall "sim_generate" simGenerate :: CString -> CString -> IO CInt

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

writeCsv :: FilePath -> SimParams -> IO ()
writeCsv path p = do
  write (path ++ ".meta") (metaText p)
  case spMode p of
    ModeCandles -> do
      rm (ticksPath path)
      write path . csvText p =<< runSimH simHooks p
    _           -> do
      cs <- withFile (ticksPath path) WriteMode $ \h -> do
        hSetBuffering h (BlockBuffering (Just (1024 * 1024)))
        hPutStr h (tickHeader (spBookDepth p) ++ "\n")
        runSimTicksH simHooks (emitTick h) p
      write path (csvText p cs)
  where
    -- a candles-mode run leaves no sidecar: drop a stale one from an
    -- earlier same-name tick-mode run, or the UI would load it instead
    rm fp = removeFile fp `catch` \e -> unless (isDoesNotExistError e) (ioError e)
    write fp s = withFile fp WriteMode $ \h -> hPutStr h s
    emitTick h t = hPutBuilder h (tickRowB (spPriceScale p) (spBookDepth p) t <> char8 '\n')
    simHooks = SimHooks
      { hookProgress = \d t -> writeIORef progressRef (d, t)
      , hookCancelled = readIORef cancelRef
      }

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
