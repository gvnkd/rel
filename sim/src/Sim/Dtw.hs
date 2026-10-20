{-# LANGUAGE BangPatterns #-}

-- Dynamic Time Warping core for cross-series shape matching. A window of
-- series A is z-normalized (prefix-sum O(1) per window, so any Y-deformation
-- of the match is factored out) and matched against z-normalized segments of
-- series B with a Sakoe-Chiba band: X-deformation up to `r` candles either
-- way. The warping path is monotone and unit-stepped by construction, so a
-- match can never map past to future. Distances are reported per path step
-- so scores are comparable across window lengths.
module Sim.Dtw
  ( Prefix
  , mkPrefix
  , pfLen
  , winMean
  , winStd
  , zslice
  , dtwPath
  , dtwPathFree
  , stepCost
  , subseqAt
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- prefix sums of x and x^2: O(1) window mean/std over raw series
data Prefix = Prefix
  { pfX  :: !(VU.Vector Double)
  , pfS  :: !(VU.Vector Double)
  , pfQ  :: !(VU.Vector Double)
  } deriving (Show)

mkPrefix :: VU.Vector Double -> Prefix
mkPrefix v = Prefix v (VU.scanl' (+) 0 v) (VU.scanl' (+) 0 (VU.map (\x -> x * x) v))

pfLen :: Prefix -> Int
pfLen = VU.length . pfX

winMean :: Prefix -> Int -> Int -> Double
winMean p j len = (pfS p VU.! (j + len) - pfS p VU.! j) / fromIntegral len

winStd :: Prefix -> Int -> Int -> Double
winStd p j len =
  let mu = winMean p j len
      var = (pfQ p VU.! (j + len) - pfQ p VU.! j) / fromIntegral len - mu * mu
  in sqrt (max 1e-12 var)

-- z-normalized window [j, j+len) of the raw series: mean 0, std 1
zslice :: Prefix -> Int -> Int -> VU.Vector Double
zslice p j len =
  let mu = winMean p j len
      sd = winStd p j len
  in VU.generate len (\k -> (pfX p VU.! (j + k) - mu) / sd)

-- banded DTW of a onto b (both already normalized the same way): full
-- consumption of both, |i-j| <= r. Returns (squared-error cost, warping
-- path from (0,0) to (m-1,n-1)); path is monotone with unit steps.
-- (inf, []) when the band cannot connect the endpoints.
dtwPath :: VU.Vector Double -> VU.Vector Double -> Int -> (Double, [(Int, Int)])
dtwPath a b r0
  | m == 0 || n == 0 = (0 / 0, [])
  | abs (m - n) > r = (1 / 0, [])
  | otherwise = case dtwMatrix a b r0 of
      Nothing -> (1 / 0, [])
      Just c -> (c VU.! ((m - 1) * n + (n - 1)), backtrack c n (m - 1) (n - 1))
  where
  m = VU.length a
  n = VU.length b
  r = max 1 r0

-- subsequence variant: a (the query) is consumed fully but the path in b
-- may END at any column within the band of the diagonal — the standard
-- free-end match used when a window should align against a reference
-- regardless of trailing content.
dtwPathFree :: VU.Vector Double -> VU.Vector Double -> Int -> (Double, [(Int, Int)])
dtwPathFree a b r0
  | m == 0 || n == 0 = (0 / 0, [])
  | null ends = (1 / 0, [])
  | otherwise = case dtwMatrix a b r0 of
      Nothing -> (1 / 0, [])
      Just c -> case ends of
        e0 : es ->
          let jEnd = foldl' (\ja jb -> if cell c jb <= cell c ja then jb else ja) e0 es
          in (cell c jEnd, backtrack c n (m - 1) jEnd)
        [] -> (1 / 0, [])
  where
  m = VU.length a
  n = VU.length b
  r = max 1 r0
  ends = [j | j <- [max 0 (m - 1 - r) .. min (n - 1) (m - 1 + r)]]
  cell c j = c VU.! ((m - 1) * n + j)

-- full banded cost matrix (cells outside the band stay at infinity)
dtwMatrix :: VU.Vector Double -> VU.Vector Double -> Int -> Maybe (VU.Vector Double)
dtwMatrix a b r0
  | m == 0 || n == 0 = Nothing
  | otherwise = Just $ runST $ do
      cost <- VUM.new (m * n)
      VUM.set cost inf
      let d i j = let x = a VU.! i - b VU.! j in x * x
          readC i j
            | j < 0 || j >= n = pure inf
            | otherwise = VUM.read cost (i * n + j)
      forM_ [0 .. min r (n - 1)] $ \j -> do
        prev <- if j == 0 then pure 0 else readC 0 (j - 1)
        VUM.write cost j (d 0 j + prev)
      forM_ [1 .. m - 1] $ \i -> do
        let lo = max 0 (i - r)
            hi = min (n - 1) (i + r)
        forM_ [lo .. hi] $ \j -> do
          p1 <- readC (i - 1) j
          p2 <- readC i (j - 1)
          p3 <- readC (i - 1) (j - 1)
          VUM.write cost (i * n + j) (d i j + min p1 (min p2 p3))
      VU.unsafeFreeze cost
  where
  m = VU.length a
  n = VU.length b
  r = max 1 r0
  inf = 1 / 0

backtrack :: VU.Vector Double -> Int -> Int -> Int -> [(Int, Int)]
backtrack c n i j = go i j []
  where
  cell k = c VU.! k
  go 0 0 acc = (0, 0) : acc
  go ri rj acc
    | ri == 0 = go 0 (rj - 1) ((ri, rj) : acc)
    | rj == 0 = go (ri - 1) 0 ((ri, rj) : acc)
    | otherwise =
        let diag = cell ((ri - 1) * n + (rj - 1))
            up = cell ((ri - 1) * n + rj)
            left = cell (ri * n + (rj - 1))
        in if diag <= up && diag <= left
             then go (ri - 1) (rj - 1) ((ri, rj) : acc)
             else if up <= left
               then go (ri - 1) rj ((ri, rj) : acc)
               else go ri (rj - 1) ((ri, rj) : acc)

-- per-path-step mean squared error: the score comparable across windows
stepCost :: (Double, [(Int, Int)]) -> Double
stepCost (total, []) = total
stepCost (total, ps) = total / fromIntegral (length ps)

-- z-normalized slice [j, j+outLen) using mean/std over [j, j+statsLen)
-- (stats from the win-length window at the candidate start, applied to
-- the whole segment — the UCR subsequence convention; normalizing over
-- the full segment would mix match and trailing content)
zwin :: Prefix -> Int -> Int -> Int -> VU.Vector Double
zwin p j statsLen outLen =
  let mu = winMean p j statsLen
      sd = winStd p j statsLen
  in VU.generate outLen (\k -> (pfX p VU.! (j + k) - mu) / sd)

-- best banded match of a z-normalized query against the z-normalized
-- segment of `ref` starting at `j0`; the segment is win+2*band long, so
-- the match may start/end up to `band` off the diagonal. Nothing when the
-- win-length window does not fit.
subseqAt :: VU.Vector Double -> Prefix -> Int -> Int -> Maybe (Double, [(Int, Int)])
subseqAt q ref j0 band
  | j0 < 0 || j0 + m > pfLen ref = Nothing
  | otherwise = Just (dtwPathFree q (zwin ref j0 m segAvail) band)
  where
  m = VU.length q
  segAvail = min (m + 2 * band) (pfLen ref - j0)
