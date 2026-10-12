module SimFFI where

import Control.Exception (SomeException, try)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Sim.Cli (applyArgs)
import Sim.Csv (csvText)
import Sim.Gen (runSim)
import Sim.Types (SimParams)
import System.IO (IOMode (WriteMode), hPutStr, withFile)

-- params are "key=value" pairs separated by ';', keys as in the CLI
-- without the leading "--" (e.g. "candles=1000;seed=7")
foreign export ccall "sim_generate" simGenerate :: CString -> CString -> IO CInt

simGenerate :: CString -> CString -> IO CInt
simGenerate paramsPtr outPtr = do
  paramsStr <- peekCString paramsPtr
  outPath   <- peekCString outPtr
  case applyArgs (toArgs paramsStr) of
    Left _  -> pure 1
    Right p -> do
      result <- try (writeCsv outPath p) :: IO (Either SomeException ())
      pure $ case result of
        Left _  -> 2
        Right _ -> 0

writeCsv :: FilePath -> SimParams -> IO ()
writeCsv path p = withFile path WriteMode $ \h -> hPutStr h (csvText p (runSim p))

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
