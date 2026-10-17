{-# LANGUAGE StrictData #-}

-- optimizer driver for SMA-cross strategies. One session = one seed: N
-- candidate strategies are sampled with splitmix (each slot's fast/slow
-- periods and position), every candidate is backtested on the val
-- segment, the best-by-Calmar one is then evaluated on the held-out test
-- segment. Fully deterministic per seed.
module Sim.Strat.Optimize
  ( OptParams (..)
  , defaultOptParams
  , OptHooks (..)
  , defaultOptHooks
  , OptResult (..)
  , OptSplits (..)
  , optSplits
  , sampleStrat
  , optimizeSession
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU
import Sim.Strat.Backtest
import Sim.Strat.Rules
import Sim.Types (SimCancelled (..))
import System.Random.SplitMix (SMGen, mkSMGen, nextWord64)

data OptParams = OptParams
  { opSlots      :: !Int     -- rule slots per strategy (fixed)
  , opCandidates :: !Int     -- candidates sampled per session
  , opValFrac    :: !Double  -- val (search) segment fraction; the rest is test
  , opFee        :: !Double  -- per position change, fraction
  , opFastMax    :: !Int     -- fastest SMA period range
  , opSlowMax    :: !Int
  } deriving (Show)

defaultOptParams :: OptParams
defaultOptParams = OptParams 4 200 0.2 0.0002 50 200

data OptHooks = OptHooks
  { ohProgress  :: Int -> Int -> IO ()
  , ohBest      :: Double -> IO ()
  , ohCancelled :: IO Bool
  }

defaultOptHooks :: OptHooks
defaultOptHooks = OptHooks (\_ _ -> pure ()) (\_ -> pure ()) (pure False)

data OptResult = OptResult
  { orSeed       :: !Word64
  , orStrat      :: !Strat
  , orCandCalmar :: !(VU.Vector Double) -- val Calmar per sampled candidate
  , orValCalmar  :: !Double             -- best candidate's val Calmar
  , orTest       :: !BacktestResult     -- best candidate on the test segment
  } deriving (Show)

data OptSplits = OptSplits
  { osVal    :: !(Int, Int)  -- backtest range for candidate search
  , osTest   :: !(Int, Int)  -- backtest range for the final report
  , osWarmup :: !Int
  } deriving (Show)

-- candle ranges. Warmup covers the slowest allowed SMA so every slot is
-- valid from the first step; val takes the first valFrac of the usable
-- candles, test the rest (needs >= 10 samples).
optSplits :: OptParams -> Int -> Either String OptSplits
optSplits p n
  | n < 60 = Left "need at least 60 candles"
  | otherwise =
      let warmup = opSlowMax p - 1
          usableN = (n - 1) - warmup
          nVal = max 10 (floor (opValFrac p * fromIntegral usableN))
          nTest = usableN - nVal
      in if nTest < 10
           then Left "not enough candles for a 10-sample test segment"
           else Right (OptSplits (warmup, warmup + nVal) (warmup + nVal, n - 1) warmup)

-- splitmix sampling; modulo bias is fine for a heuristic search
sampleStrat :: OptParams -> SMGen -> (SMGen, Strat)
sampleStrat p g0 = go g0 (max 1 (opSlots p)) []
  where
    go g 0 acc = (g, Strat (reverse acc))
    go g k acc =
      let (g1, fast) = rollInt g (2, max 2 (opFastMax p))
          (g2, slow) = rollInt g1 (fast + 1, max (fast + 1) (opSlowMax p))
          (g3, pIdx) = rollInt g2 (0, 2)
      in go g3 (k - 1) (Slot fast slow (pIdx - 1) : acc)
    rollInt g (lo, hi) =
      let (w, g') = nextWord64 g
      in (g', lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1)))

optimizeSession :: OptHooks -> OptParams -> VU.Vector Double -> Word64 -> IO OptResult
optimizeSession hooks p closes seed =
  case optSplits p (VU.length closes) of
    Left err -> ioError (userError err)
    Right sp -> loop (mkSMGen seed) 1 Nothing (-1 / 0) [] sp
  where
    candVal sp st =
      let bt = backtest (opFee p) closes (evalSignal st closes) (osVal sp)
      in bmCalmar (brMetrics bt)
    loop g k bestS bestC hist sp
      | k > max 1 (opCandidates p) = do
          let st = case bestS of
                Just s  -> s
                Nothing -> Strat []
              testRes = backtest (opFee p) closes (evalSignal st closes) (osTest sp)
          pure (OptResult seed st (VU.fromList (reverse hist)) bestC testRes)
      | otherwise = do
          cancelled <- ohCancelled hooks
          when cancelled (throwIO SimCancelled)
          let (g', st) = sampleStrat p g
              cm = candVal sp st
          ohProgress hooks k (max 1 (opCandidates p))
          ohBest hooks cm
          if cm > bestC
            then loop g' (k + 1) (Just st) cm (cm : hist) sp
            else loop g' (k + 1) bestS bestC (cm : hist) sp
