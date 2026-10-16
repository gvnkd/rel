{-# LANGUAGE StrictData #-}

-- offline training session driver. One session = one seed: an MLP is
-- trained full-batch on the train segment, each epoch is ranked on the
-- validation segment by the policy Calmar (val PnL / max drawdown), the
-- best epoch's model is then evaluated on the held-out test segment.
-- Reward during training is plain next-candle PnL (minus fees); drawdown
-- only enters through model selection.
module Sim.ML.Train
  ( TrainParams (..)
  , defaultTrainParams
  , TrainHooks (..)
  , defaultTrainHooks
  , Metrics (..)
  , SessionResult (..)
  , MlData (..)
  , prepareData
  , trainSession
  , evalPolicy
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.ML.Data
import Sim.ML.Model
import Sim.Types (SimCancelled (..))
import System.Random.SplitMix (mkSMGen)

data TrainParams = TrainParams
  { tpEpochs     :: !Int
  , tpLR         :: !Double
  , tpHidden     :: ![Int]
  , tpTrainFrac  :: !Double
  , tpValFrac    :: !Double
  , tpFee        :: !Double
  , tpSpec       :: !FeatureSpec
  } deriving (Show)

defaultTrainParams :: TrainParams
defaultTrainParams = TrainParams
  { tpEpochs    = 50
  , tpLR        = 0.05
  , tpHidden    = [16]
  , tpTrainFrac = 0.6
  , tpValFrac   = 0.2
  , tpFee       = 0.0002
  , tpSpec      = defaultFeatureSpec
  }

data TrainHooks = TrainHooks
  { thProgress  :: Int -> Int -> IO ()
  , thBest      :: Double -> IO ()
  , thCancelled :: IO Bool
  }

defaultTrainHooks :: TrainHooks
defaultTrainHooks = TrainHooks (\_ _ -> pure ()) (\_ -> pure ()) (pure False)

data Metrics = Metrics
  { mPnl      :: !Double
  , mMaxDD    :: !Double
  , mCalmar   :: !Double
  , mAccuracy :: !Double
  , mTrades   :: !Int
  } deriving (Show)

data SessionResult = SessionResult
  { srSeed      :: !Word64
  , srEpochs    :: !Int
  , srBestEpoch :: !Int
  , srValCalmar :: !Double
  , srMetrics   :: !Metrics       -- test segment, best-on-val model
  , srEquity    :: !(VU.Vector Double)
  } deriving (Show)

-- preprocessed, normalized dataset ready for sessions
data MlData = MlData
  { mdCloses :: !(VU.Vector Double)
  , mdRows   :: !(V.Vector (VU.Vector Double))
  , mdLabels :: !(VU.Vector Int)
  , mdTrain  :: ![Int]
  , mdVal    :: ![Int]
  , mdTest   :: ![Int]
  } deriving (Show)

-- feature-build + split + normalize (train-segment mean/std applied to
-- all segments). Needs enough candles for non-empty train/val/test.
prepareData :: TrainParams -> [Bar] -> Either String MlData
prepareData tp bars
  | length bars < 60 = Left "need at least 60 candles"
  | otherwise =
      let fs = buildFeatures (tpSpec tp) bars
          n = fCount fs
          usable = [fWarmup fs .. n - 2]
          nt = length usable
          nTrain = max 10 (floor (tpTrainFrac tp * fromIntegral nt))
          nVal   = max 10 (floor (tpValFrac tp * fromIntegral nt))
          nTest  = nt - nTrain - nVal
      in if nTest < 10
           then Left "not enough candles for a 10-sample test segment"
           else
             let (tr, rest) = splitAt nTrain usable
                 (va, te)   = splitAt nVal rest
                 closes = VU.fromList (map barC bars)
                 rows0 = fRows fs
                 (means, stds) = colStats rows0 tr
                 rows = V.map (normRow means stds) rows0
             in Right (MlData closes rows (fLabels fs) tr va te)
  where
    size = featureSize (tpSpec tp)
    colStats rows idxs =
      let used = [rows V.! i | i <- idxs, VU.length (rows V.! i) == size]
          m = VU.generate size $ \k ->
                sum [r VU.! k | r <- used] / fromIntegral (max 1 (length used))
          sd = VU.generate size $ \k ->
                let v = sum [ (r VU.! k - m VU.! k) ^ 2 | r <- used] / fromIntegral (max 1 (length used))
                in max 1e-8 (sqrt v)
      in (m, sd)
    normRow m sd r
      | VU.length r == size = VU.zipWith3 (\x a b -> (x - a) / b) r m sd
      | otherwise = r

-- policy: predicted class at candle i -> position held over i -> i+1
-- (0 = down -> short, 1 = flat, 2 = up -> long); PnL = position * next
-- return - fee * |position change|. Returns metrics + the equity curve.
evalPolicy :: Model -> Double -> MlData -> [Int] -> (Metrics, VU.Vector Double)
evalPolicy m fee md idxs =
  let steps = [ (i, predictClass m (mdRows md V.! i), mdLabels md VU.! i)
              | i <- idxs, VU.length (mdRows md V.! i) > 0 ]
      go _ acc [] = acc
      go prevPos (eq, peak, pnl, dd, correct, total, trades, curve) ((i, cls, lab) : rest) =
        let pos = cls - 1
            r = mdCloses md VU.! (i + 1) / mdCloses md VU.! i - 1
            step = fromIntegral pos * r - fee * fromIntegral (abs (pos - prevPos))
            eq' = eq + step
            peak' = max peak eq'
            dd' = max dd (peak' - eq')
            correct' = correct + (if cls == lab then 1 else 0)
            trades' = trades + (if pos /= prevPos then 1 else 0)
        in go pos (eq', peak', pnl + step, dd', correct', total + 1, trades', eq' : curve) rest
      (_eqF, _peakF, pnlF, ddF, correctF, totalF, tradesF, curveRev) =
        go 0 (0, 0, 0, 0, 0, 0, 0, []) steps
      met = Metrics pnlF ddF (if ddF > 1e-12 then pnlF / ddF else pnlF)
                    (if totalF > 0 then fromIntegral correctF / fromIntegral totalF else 0)
                    tradesF
  in (met, VU.fromList (reverse curveRev))

-- one training session; returns Nothing when cancelled. Model selection
-- is on validation Calmar; the returned result is the TEST evaluation of
-- the selected model.
trainSession :: TrainHooks -> TrainParams -> MlData -> Word64 -> IO (Maybe SessionResult)
trainSession hooks tp md seed = do
  let g0 = mkSMGen seed
      sizes = featureSize (tpSpec tp) : tpHidden tp ++ [3]
      m0 = initModel g0 sizes
      trainSamples = [ (mdRows md V.! i, mdLabels md VU.! i) | i <- mdTrain md ]
      loop :: Model -> Int -> Maybe Model -> Double -> Int -> IO (Maybe SessionResult)
      loop m e bestM bestCalmar bestEp
        | e > tpEpochs tp = finish bestM bestCalmar bestEp
        | otherwise = do
            cancelled <- thCancelled hooks
            when cancelled (throwIO SimCancelled)
            let m' = sgdStep (tpLR tp) m trainSamples
                (valMet, _) = evalPolicy m' (tpFee tp) md (mdVal md)
                vc = mCalmar valMet
            thProgress hooks e (tpEpochs tp)
            thBest hooks vc
            if vc > bestCalmar
              then loop m' (e + 1) (Just m') vc e
              else loop m' (e + 1) bestM bestCalmar bestEp
      finish bestM bestCalmar bestEp =
        let mFinal = case bestM of
              Just bm -> bm
              Nothing -> m0
            (met, eq) = evalPolicy mFinal (tpFee tp) md (mdTest md)
        in pure (Just (SessionResult seed (tpEpochs tp) bestEp bestCalmar met eq))
  loop m0 1 Nothing (-1 / 0) 0
