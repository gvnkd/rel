{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim.Entropy (permEntropy, rollingPermEntropy) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.List (sortBy)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- ordinal-pattern (Bandt-Pompe) permutation entropy, normalized to
-- [0, 1] by H / ln(m!): 1 for an unconstrained (iid-noise-like) series,
-- near 0 for a monotone one. Ties are broken by position so every window
-- has exactly one pattern
maxM :: Int
maxM = 7

-- normalized entropy of the whole series
permEntropy :: Int -> [Double] -> Double
permEntropy m0 xs
  | VU.null ids = 0 / 0
  | otherwise = (log t - s / t) / lnFact
  where
    m = clamp 2 maxM m0
    ids = patternIds m (VU.fromList xs)
    t = fromIntegral (VU.length ids)
    s = sum [f c | c <- countsOf (fact m) (VU.toList ids)]
    lnFact = log (fromIntegral (fact m))
    f 0 = 0
    f c = fromIntegral c * log (fromIntegral c)

-- per-candle rolling permutation entropy over a sliding window of
-- `win` closes: the result has one entry per input candle, NaN until
-- the window fills (or everywhere when win < m / win < 2 / series too
-- short). Incremental: the pattern count per window is constant
-- (win - m + 1), so H = ln T - (sum c*ln c)/T and the sliding update
-- touches only the two changed bins
rollingPermEntropy :: Int -> Int -> [Double] -> [Double]
rollingPermEntropy win m0 xs
  | win < m || win < 2 || cnt < 1 || n < win = replicate n (0 / 0)
  | otherwise = runST compute
  where
  compute :: forall s. ST s [Double]
  compute = do
      counts <- newArray (0, k - 1) 0 :: ST s (STUArray s Int Int)
      forM_ [0 .. cnt - 1] $ \j -> do
        c <- readArray counts (ids VU.! j)
        writeArray counts (ids VU.! j) (c + 1)
      s0 <- foldl' (+) 0 <$> mapM (f' counts) [0 .. k - 1]
      out <- VUM.new n
      forM_ [0 .. win - 2] $ \i -> VUM.write out i (0 / 0)
      VUM.write out (win - 1) ((log t - s0 / t) / lnFact)
      let go :: Int -> Double -> ST s ()
          go !i !s
            | i >= n = pure ()
            | otherwise = do
                cold <- readArray counts (ids VU.! (i - win))
                writeArray counts (ids VU.! (i - win)) (cold - 1)
                cnew <- readArray counts (ids VU.! (i - m + 1))
                writeArray counts (ids VU.! (i - m + 1)) (cnew + 1)
                let s' = s - f cold + f (cold - 1) + f (cnew + 1) - f cnew
                VUM.write out i ((log t - s' / t) / lnFact)
                go (i + 1) s'
      go win s0
      VU.toList <$> VU.unsafeFreeze out

  n = length xs
  m = clamp 2 maxM m0
  k = fact m
  cnt = win - m + 1
  t = fromIntegral cnt
  lnFact = log (fromIntegral k)
  ids = patternIds m (VU.fromList xs)
  f' counts j = f <$> readArray counts j
  f 0 = 0
  f c = fromIntegral c * log (fromIntegral c)

-- ordinal pattern id (Lehmer code) of the m values at a window start
patternIds :: Int -> VU.Vector Double -> VU.Vector Int
patternIds m v
  | m < 2 || n < m = VU.empty
  | otherwise = VU.generate (n - m + 1) (\p -> patId m (VU.slice p m v))
  where
    n = VU.length v

patId :: Int -> VU.Vector Double -> Int
patId m v = lehmer [j | (_, j) <- sorted]
  where
    keyed = [(v VU.! j, j) | j <- [0 .. m - 1]]
    sorted = sortBy (\(a, i) (b, j) -> compare a b <> compare i j) keyed
    lehmer js = fst (foldr step (0, []) (zip [0 ..] js))
    step (pos, j) (acc, seen) = (acc + length (filter (< j) seen) * fact (m - 1 - pos), j : seen)

countsOf :: Int -> [Int] -> [Int]
countsOf k ids = [ length (filter (== j) ids) | j <- [0 .. k - 1] ]

fact :: Int -> Int
fact k = product [1 .. k]

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
