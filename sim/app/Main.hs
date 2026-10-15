module Main (main) where

import Sim.Cli (applyArgs)
import Sim.Csv (csvText)
import Sim.Gen (runSim)
import Sim.Sinus (runSimSinus)
import Sim.Types (SimParams (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case applyArgs args of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right params -> putStr (csvText params (choose params))
  where
    choose p
      | spSinus p = runSimSinus p
      | otherwise = runSim p
