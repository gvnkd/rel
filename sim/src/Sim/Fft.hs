{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim.Fft (fftApprox, solveLS, gram, corr) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray, getElems)
import Data.Bits (countTrailingZeros, setBit, testBit)
import Data.Complex (Complex ((:+)), cis, magnitude)
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

minWindow :: Int
minWindow = 16

-- fits the window [start, end) of `series` with `waves` harmonics and
-- extrapolates `predict` points past its end; returns (fit, forecast).
-- waves = 0 picks an automatic count; the window is clamped to the
-- series, ([], []) on invalid input
fftApprox :: Int -> [Double] -> Int -> Int -> Int -> ([Double], [Double])
fftApprox waves series start end predict
  | predict < 1 = ([], [])
  | n < minWindow = ([], [])
  | otherwise = (VU.toList (VU.take n vals), VU.toList (VU.drop n vals))
  where
    len = length series
    lo = clamp 0 (len - 1) start
    hi = clamp (lo + 1) len end
    n = hi - lo
    vys = VU.fromList (take n (drop lo series))
    mu = VU.sum vys / fromIntegral n
    vysC = VU.map (subtract mu) vys
    freqs = topFreqs n vysC (resolveWaves waves n)
    pairs = zip freqs (pairUp (solveLS n (VU.toList vysC) freqs))
    vals = synth (n + predict) mu pairs

-- accumulates mu + the fitted waves into one unboxed vector; per point
-- the rotation recurrence advances cos/sin with 4 multiplies, no trig
synth :: Int -> Double -> [(Double, (Double, Double))] -> VU.Vector Double
synth count mu pairs = runST $ do
  out <- VUM.replicate count mu
  forM_ pairs $ \(f, (a, b)) -> do
    let cw = cos (2 * pi * f)
        sw = sin (2 * pi * f)
        go !i !c !s
          | i >= count = pure ()
          | otherwise = do
              x <- VUM.read out i
              VUM.write out i (x + a * c + b * s)
              go (i + 1) (c * cw - s * sw) (s * cw + c * sw)
    go 0 1 0
  VU.unsafeFreeze out

resolveWaves :: Int -> Int -> Int
resolveWaves 0 n = clamp 1 (n `div` 16) 8
resolveWaves w n = clamp 1 (n `div` 4) w

pairUp :: [a] -> [(a, a)]
pairUp (a : b : rest) = (a, b) : pairUp rest
pairUp _ = []

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- top-k spectral bins (by magnitude) of the zero-padded window, snapped
-- to exact window DFT bins (f = q / n) and de-duplicated; the snapped
-- cos/sin columns are exactly orthogonal over the window, so the least
-- squares solve cannot produce runaway coefficients. A hard low-pass
-- drops bins with period under 8 candles: on candle data the upper
-- spectrum is per-candle noise, and fitting it makes the forecast jitter
-- instead of continuing the visible structure (iterative in-place
-- unboxed FFT)
topFreqs :: Int -> VU.Vector Double -> Int -> [Double]
topFreqs n vys k =
  let m = nextPow2 n
      spec = fftV m vys
      bins = [ (j, magnitude (spec VU.! j)) | j <- [1 .. m `div` 2] ]
      top = take k (sortOn (negate . snd) bins)
      snapped = dedup [ round (fromIntegral j * fromIntegral n / fromIntegral m) | (j, _) <- top ]
  in [ fromIntegral q / fromIntegral n | q <- snapped, q > 0, 2 * q < n, q <= n `div` 8 ]
  where
    dedup = foldl' add []
    add seen q
      | q `elem` seen = seen
      | otherwise = seen ++ [q]

fftV :: Int -> VU.Vector Double -> VU.Vector (Complex Double)
fftV m vys = runST $ do
  a <- VUM.new m
  forM_ [0 .. m - 1] $ \i ->
    VUM.write a (bitrev lg i) (if i < VU.length vys then vys VU.! i :+ 0 else 0 :+ 0)
  let loop !len
        | len > m = pure ()
        | otherwise = do
            let half = len `div` 2
            forM_ [0, len .. m - 1] $ \base ->
              forM_ [0 .. half - 1] $ \j -> do
                let w = cis (-2 * pi * fromIntegral j / fromIntegral len)
                x <- VUM.read a (base + j)
                y <- VUM.read a (base + j + half)
                VUM.write a (base + j) (x + w * y)
                VUM.write a (base + j + half) (x - w * y)
            loop (len * 2)
  loop 2
  VU.unsafeFreeze a
  where
    lg = countTrailingZeros m
    bitrev bits x = foldl' step 0 [0 .. bits - 1]
      where
        step acc i = if testBit x i then setBit acc (bits - 1 - i) else acc

nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go p = if p >= n then p else go (p * 2)

-- correlations of ys with cos(w t) and sin(w t) via the rotation
-- recurrence: O(n) multiplies, one trig evaluation per frequency
corr :: Int -> [Double] -> Double -> (Double, Double)
corr _ ys w = go 1 0 0 0 ys
  where
    cw = cos w
    sw = sin w
    go _ _ cc ss [] = (cc, ss)
    go c s cc ss (y : rest) = go (c * cw - s * sw) (s * cw + c * sw) (cc + y * c) (ss + y * s) rest

-- strict unboxed variant used on the hot path
corrV :: VU.Vector Double -> Double -> (Double, Double)
corrV vys w = go 0 1 0 0 0
  where
    n = VU.length vys
    cw = cos w
    sw = sin w
    go !i !c !s !cc !ss
      | i >= n = (cc, ss)
      | otherwise = go (i + 1) (c * cw - s * sw) (s * cw + c * sw) (cc + vys VU.! i * c) (ss + vys VU.! i * s)

-- Gram matrix of the cos/sin design columns in closed form via the
-- Dirichlet kernel: O(k^2), independent of the window length
gram :: Int -> [Double] -> [[Double]]
gram n ws = [ [ entry i j | j <- [0 .. m - 1] ] | i <- [0 .. m - 1] ]
  where
    m = 2 * length ws
    n' = fromIntegral n
    w i = ws !! (i `div` 2)
    entry i j
      | i == j = if even i then n' / 2 + d (2 * wi) / 2 else n' / 2 - d (2 * wi) / 2
      | even i && even j = (d (wi - wj) + d (wi + wj)) / 2
      | odd i && odd j = (d (wi - wj) - d (wi + wj)) / 2
      | even i = (s (wi + wj) + s (wj - wi)) / 2
      | otherwise = (s (wi + wj) + s (wi - wj)) / 2
      where
        wi = w i
        wj = w j
    d x
      | abs x < 1e-12 = n'
      | otherwise = sin (n' * x / 2) * cos ((n' - 1) * x / 2) / sin (x / 2)
    s x
      | abs x < 1e-12 = 0
      | otherwise = sin (n' * x / 2) * sin ((n' - 1) * x / 2) / sin (x / 2)

-- least squares fit of cos/sin columns at the given frequencies (cycles
-- per sample) against ys; the Gram matrix is eigendecomposed (Jacobi) and
-- eigen-directions below 1e-10 of the largest eigenvalue are truncated,
-- so collinear columns (high wave counts, padded FFT bins) cannot produce
-- the huge coefficients that make extrapolation explode
solveLS :: Int -> [Double] -> [Double] -> [Double]
solveLS n ys freqs = x
  where
    ws = map (2 * pi *) freqs
    g = gram n ws
    vys = VU.fromList ys
    b = concatMap (\w -> let (c, s) = corrV vys w in [c, s]) ws
    (evals, evecs) = jacobi g
    lmax = maximum (map abs evals)
    lam = (1e-6 * lmax) ^ 2
    coef (l, v)
      | l > 1e-10 * lmax = dot v b * l / (l * l + lam)
      | otherwise = 0
    x = [ sum [ v !! i * coef (l, v) | (l, v) <- zip evals evecs ] | i <- [0 .. length g - 1] ]
    dot a c = sum (zipWith (*) a c)

-- eigendecomposition of a small symmetric matrix (in-place Givens
-- rotations); returns eigenvalues and the matching eigenvectors
jacobi :: [[Double]] -> ([Double], [[Double]])
jacobi a0 = runST go
  where
    m = length a0
    pairs = [ (p, q) | p <- [0 .. m - 1], q <- [p + 1 .. m - 1] ]
    sq x = x * x
    go :: forall s. ST s ([Double], [[Double]])
    go = do
      a <- newListArray ((0, 0), (m - 1, m - 1)) (concat a0) :: ST s (STUArray s (Int, Int) Double)
      v <- newListArray ((0, 0), (m - 1, m - 1)) [if i == j then 1.0 else 0.0 | i <- [0 .. m - 1], j <- [0 .. m - 1]] :: ST s (STUArray s (Int, Int) Double)
      let sweep = forM_ pairs $ \(p, q) -> do
            apq <- readArray a (p, q)
            when (abs apq > 1e-300) $ do
              app <- readArray a (p, p)
              aqq <- readArray a (q, q)
              let theta = 0.5 * atan2 (2 * apq) (aqq - app)
                  c = cos theta
                  s = sin theta
              forM_ [0 .. m - 1] $ \i ->
                when (i /= p && i /= q) $ do
                  aip <- readArray a (i, p)
                  aiq <- readArray a (i, q)
                  writeArray a (i, p) (c * aip - s * aiq)
                  writeArray a (i, q) (s * aip + c * aiq)
                  writeArray a (p, i) (c * aip - s * aiq)
                  writeArray a (q, i) (s * aip + c * aiq)
              writeArray a (p, p) (c * c * app + s * s * aqq - 2 * c * s * apq)
              writeArray a (q, q) (s * s * app + c * c * aqq + 2 * c * s * apq)
              writeArray a (p, q) 0
              writeArray a (q, p) 0
              forM_ [0 .. m - 1] $ \i -> do
                vip <- readArray v (i, p)
                viq <- readArray v (i, q)
                writeArray v (i, p) (c * vip - s * viq)
                writeArray v (i, q) (s * vip + c * viq)
          loop it
            | it > (200 :: Int) = pure ()
            | otherwise = do
                off2 <- sum <$> mapM (\(p, q) -> sq <$> readArray a (p, q)) pairs
                diag2 <- sum <$> mapM (\i -> sq <$> readArray a (i, i)) [0 .. m - 1]
                if off2 <= 1e-30 * (diag2 + 1e-300)
                  then pure ()
                  else sweep >> loop (it + 1)
      loop 0
      evals <- mapM (\i -> readArray a (i, i)) [0 .. m - 1]
      vels <- getElems v
      let evecs = [ [ vels !! (i * m + k) | i <- [0 .. m - 1] ] | k <- [0 .. m - 1] ]
      pure (evals, evecs)
