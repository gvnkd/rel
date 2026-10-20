-- CLI for the DTW correlation scanner: slide windows over series A and
-- find best-matching parts of series B at every requested window length,
-- optionally across a timeframe-permutation grid.
--
--   dtw --dataa EURUSDM1.csv --datab signal.csv --out res \
--       [--windows 64,128,256,512] [--band N] [--step N] [--maxcand N] \
--       [--paa 8] [--card 8] [--tfs 1m,5m,1h]
--
-- Single-pair mode is the default; --tfs switches to grid mode (both
-- --dataa/--datab become comma-separated path lists aligned with --tfs,
-- empty entries skip that timeframe). Multicore is on by default
-- (-with-rtsopts=-N); artifacts: <out>/dtw_<tfA>_<tfB>_<win>.json + .csv.
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (forM, forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Sim.DtwIndex as DTW
import Sim.Dtw (Prefix, mkPrefix, pfLen)
import qualified Sim.ML.Data as MLD
import qualified Sim.Run as Run
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

data Opts = Opts
  { oDataA  :: String
  , oDataB  :: String
  , oOut    :: String
  , oTfs    :: Maybe [String]
  , oWins   :: [Int]
  , oBand   :: Int
  , oStep   :: Int
  , oMaxC   :: Int
  , oPaa    :: Int
  , oCard   :: Int
  }

defaultOpts :: Opts
defaultOpts = Opts "" "" "" Nothing [128] 0 0 64 8 8

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case parseArgs args defaultOpts of
    Left err -> do
      putStrLn ("error: " ++ err)
      putStrLn "usage: dtw --dataa P [--datab P] --out DIR [--windows 64,128] [--band N] [--step N] [--maxcand N] [--paa N] [--card N] [--tfs 1m,5m,...]"
      exitFailure
    Right o
      | null (oDataA o) || null (oOut o) -> do
          putStrLn "error: --dataa and --out are required"
          exitFailure
      | otherwise -> runScan o

parseArgs :: [String] -> Opts -> Either String Opts
parseArgs [] o = Right o
parseArgs (a : rest) o = case a of
  "--dataa"  -> withArg rest (\v r -> parseArgs r o { oDataA = v })
  "--datab"  -> withArg rest (\v r -> parseArgs r o { oDataB = v })
  "--out"    -> withArg rest (\v r -> parseArgs r o { oOut = v })
  "--windows" -> withArg rest (\v r -> parseArgs r o { oWins = csvInts v })
  "--tfs"    -> withArg rest (\v r -> parseArgs r o { oTfs = Just (splitCsv v) })
  "--band"   -> withArg rest (\v r -> parseArgs r o { oBand = readDef 0 v })
  "--step"   -> withArg rest (\v r -> parseArgs r o { oStep = readDef 0 v })
  "--maxcand" -> withArg rest (\v r -> parseArgs r o { oMaxC = readDef 64 v })
  "--paa"    -> withArg rest (\v r -> parseArgs r o { oPaa = readDef 8 v })
  "--card"   -> withArg rest (\v r -> parseArgs r o { oCard = readDef 8 v })
  _ | Just v <- stripPrefix "--dataa=" a -> parseArgs rest o { oDataA = v }
    | Just v <- stripPrefix "--datab=" a -> parseArgs rest o { oDataB = v }
    | Just v <- stripPrefix "--out=" a -> parseArgs rest o { oOut = v }
    | Just v <- stripPrefix "--windows=" a -> parseArgs rest o { oWins = csvInts v }
    | Just v <- stripPrefix "--tfs=" a -> parseArgs rest o { oTfs = Just (splitCsv v) }
    | Just v <- stripPrefix "--band=" a -> parseArgs rest o { oBand = readDef 0 v }
    | Just v <- stripPrefix "--step=" a -> parseArgs rest o { oStep = readDef 0 v }
    | Just v <- stripPrefix "--maxcand=" a -> parseArgs rest o { oMaxC = readDef 64 v }
    | Just v <- stripPrefix "--paa=" a -> parseArgs rest o { oPaa = readDef 8 v }
    | Just v <- stripPrefix "--card=" a -> parseArgs rest o { oCard = readDef 8 v }
    | otherwise -> Left ("unknown argument " ++ a)
  where
  withArg (v : r) k = k v r
  withArg [] _ = Left ("missing value after " ++ a)
  stripPrefix p s =
    if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

splitCsv :: String -> [String]
splitCsv v = map trim (split ',' v)

csvInts :: String -> [Int]
csvInts v = [ x | Just x <- map readI (split ',' v) ]

readI :: String -> Maybe Int
readI s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

readDef :: Int -> String -> Int
readDef d s = fromMaybe d (readI s)

trim :: String -> String
trim = dropWhile isSp . reverse . dropWhile isSp . reverse
  where
  isSp c = c == ' ' || c == '\t'

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (pre, [])       -> [pre]
  (pre, _ : post) -> pre : split c post

runScan :: Opts -> IO ()
runScan o = do
  caps <- getNumCapabilities
  putStrLn ("capabilities=" ++ show caps)
  let (tfsA, tfsB, pathsA, pathsB) = case oTfs o of
        Just tfs -> (tfs, tfs, splitCsv (oDataA o), splitCsv (oDataB o))
        Nothing  -> (["A"], ["B"], [oDataA o], [oDataB o])
  cacheRef <- newIORef Map.empty
  loadedA <- mapM (loadPrefix cacheRef) pathsA
  loadedB <- mapM (loadPrefix cacheRef) pathsB
  let pairsA = catMaybes (zipWith (\tf mv -> fmap (\v -> (tf, v)) mv) tfsA loadedA)
      pairsB = catMaybes (zipWith (\tf mv -> fmap (\v -> (tf, v)) mv) tfsB loadedB)
  when (null pairsA || null pairsB) $ do
    putStrLn "error: no usable series (check data paths)"
    exitFailure
  let plans = Run.dtwJobs pairsA pairsB (oWins o) (oBand o) (oStep o) (oMaxC o) (oPaa o) (oCard o)
      planSp pl = DTW.ScanParams (Run.dpWin pl) (Run.dpBand pl) (Run.dpPaa pl) (Run.dpCard pl) (Run.dpStep pl) (Run.dpMaxCand pl)
      planNWin (pl, pfa, pfb) =
        if DTW.scanValid (planSp pl) (pfLen pfa) (pfLen pfb)
          then length [0, Run.dpStep pl .. pfLen pfa - Run.dpWin pl]
          else 0
  putStrLn ("sessions=" ++ show (length plans))
  refs <- forM plans $ \job@(pl, _, _) -> do
    d <- newIORef 0
    pure (job, d, planNWin job)
  let mkHooks (job@(pl, _, _), d, total) = Run.DtwHooks
        { Run.dwProgress = \done _ -> do
            writeIORef d done
            when (done `mod` 200 == 0) $ do
              putStr ("\r" ++ Run.dpTfA pl ++ "/" ++ Run.dpTfB pl ++ " w" ++ show (Run.dpWin pl)
                      ++ " " ++ show (100 * done `div` max 1 total) ++ "%   ")
        , Run.dwBest = \_ -> pure ()
        , Run.dwCancelled = pure False
        }
      jobs = [ (pl, pfa, pfb, mkHooks r) | r@((pl, pfa, pfb), _, _) <- refs ]
  Run.runDtwBatch (oOut o) jobs
  putStrLn ""
  forM_ plans $ \(pl, _, _) -> do
    let base = oOut o ++ "/dtw_" ++ Run.dpTfA pl ++ "_" ++ Run.dpTfB pl ++ "_" ++ show (Run.dpWin pl)
    done <- doesFileExist (base ++ ".json")
    if done
      then putStrLn ("wrote " ++ base ++ ".{json,csv}")
      else putStrLn ("no result for " ++ Run.dpTfA pl ++ "/" ++ Run.dpTfB pl ++ " w" ++ show (Run.dpWin pl))
  exitSuccess
  where
  loadPrefix :: IORef (Map.Map FilePath Prefix) -> FilePath -> IO (Maybe Prefix)
  loadPrefix _ "" = pure Nothing
  loadPrefix cacheRef path = do
    cached <- readIORef cacheRef
    case Map.lookup path cached of
      Just v  -> pure (Just v)
      Nothing -> do
        putStrLn ("loading " ++ path)
        parsed <- MLD.loadCloses path
        case parsed of
          Left err   -> do
            putStrLn ("  skipped: " ++ err)
            pure Nothing
          Right cls -> do
            let v = mkPrefix cls
            modifyIORef' cacheRef (Map.insert path v)
            pure (Just v)
