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
  , shuffleIdx
  , chunkList
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.List (foldl', sortOn)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.ML.Data
import Sim.ML.Model
import Sim.Types (SimCancelled (..))
import System.Random.SplitMix (SMGen, mkSMGen, nextWord64)

data TrainParams = TrainParams
  { tpEpochs     :: !Int
  , tpLR         :: !Double
  , tpBatch      :: !Int
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
  , tpBatch     = 256
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
                  -- force the stats BEFORE the mapped vector: otherwise
                  -- V.map's stream fusion sinks the colStats let into the
                  -- per-element loop and every row recomputes mean/std
                  -- over the whole train segment (measured: 33s -> 0.02s)
                  (means, stds) = forceStats (colStats rows0 tr)
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
    forceStats p@(m, sd) = VU.sum m `seq` VU.sum sd `seq` p

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

-- one training session; returns Nothing when cancelled. Each epoch
-- shuffles the train segment (splitmix, seeded per session — fully
-- deterministic) and runs mini-batch SGD; the epoch's model is ranked on
-- validation Calmar and the best one's test evaluation is returned.
trainSession :: TrainHooks -> TrainParams -> MlData -> Word64 -> IO (Maybe SessionResult)
trainSession hooks tp md seed = do
  let g0 = mkSMGen seed
      sizes = featureSize (tpSpec tp) : tpHidden tp ++ [3]
      m0 = initModel g0 sizes
      rowsOf i = (mdRows md V.! i, mdLabels md VU.! i)
      loop :: Model -> SMGen -> Int -> Maybe Model -> Double -> Int -> IO (Maybe SessionResult)
      loop m g e bestM bestCalmar bestEp
        | e > tpEpochs tp = finish bestM bestCalmar bestEp
        | otherwise = do
            cancelled <- thCancelled hooks
            when cancelled (throwIO SimCancelled)
            let (g1, shuf) = shuffleIdx g (mdTrain md)
                batches = chunkList (max 1 (tpBatch tp)) shuf
                m' = foldl'
                       (\mm idxs -> sgdStep (tpLR tp) mm (map rowsOf idxs))
                       m batches
                (valMet, _) = evalPolicy m' (tpFee tp) md (mdVal md)
                vc = mCalmar valMet
            thProgress hooks e (tpEpochs tp)
            thBest hooks vc
            if vc > bestCalmar
              then loop m' g1 (e + 1) (Just m') vc e
              else loop m' g1 (e + 1) bestM bestCalmar bestEp
      finish bestM bestCalmar bestEp =
        let mFinal = case bestM of
              Just bm -> bm
              Nothing -> m0
            (met, eq) = evalPolicy mFinal (tpFee tp) md (mdTest md)
        in pure (Just (SessionResult seed (tpEpochs tp) bestEp bestCalmar met eq))
  loop m0 g0 1 Nothing (-1 / 0) 0

-- deterministic Fisher-Yates-free shuffle: sort by a splitmix key
shuffleIdx :: SMGen -> [Int] -> (SMGen, [Int])
shuffleIdx g0 is =
  let (g1, keys) = rollKeys g0 (length is)
  in (g1, map snd (sortOn fst (zip keys is)))
  where
    rollKeys g 0 = (g, [])
    rollKeys g n =
      let (w, g') = nextWord64 g
          (g'', rest) = rollKeys g' (n - 1)
      in (g'', w : rest)

chunkList :: Int -> [a] -> [[a]]
chunkList k = go
  where
    go [] = []
    go xs = let (a, b) = splitAt k xs in a : go b
