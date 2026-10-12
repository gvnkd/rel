module Main (main) where

import Sim.Cli (applyArgs)
import Sim.Csv (csvText)
import Sim.Gen (runSim)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case applyArgs args of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right params -> putStr (csvText params (runSim params))
