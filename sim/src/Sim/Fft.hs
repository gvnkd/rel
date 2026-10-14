{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim.Fft (fftApprox, fftBest, fftForecast, solveLS, gram, gramW, corr, corrWV, rampMass) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray, getElems)
import Data.Bits (countTrailingZeros, setBit, testBit)
import Data.Complex (Complex ((:+)), cis, conjugate, imagPart, magnitude, realPart)
import Data.List (foldl', minimumBy, sortOn)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

minWindow :: Int
minWindow = 16

-- fits the window [start, end) of `series` with `waves` harmonics and
-- extrapolates `predict` points past its end; returns (fit, forecast).
-- waves = 0 picks an automatic count; edgeW > 1 endpoint-weights the
-- least squares fit (ramps over n/8 candles at both window edges), so
-- the curve hugs the first/last candles; the window is clamped to the
-- series, ([], []) on invalid input
fftApprox :: Int -> Double -> [Double] -> Int -> Int -> Int -> ([Double], [Double])
fftApprox waves edgeW series start end predict
  | predict < 1 = ([], [])
  | Just (n, mu, pairs) <- core waves edgeW series start end =
      let vals = synth (n + predict) mu pairs
      in (VU.toList (VU.take n vals), VU.toList (VU.drop n vals))
  | otherwise = ([], [])

-- forecast only, without synthesizing the in-window fit (half the work;
-- used by the parameter search)
fftForecast :: Int -> Double -> [Double] -> Int -> Int -> Int -> [Double]
fftForecast waves edgeW series start end predict
  | predict < 1 = []
  | Just (n, mu, pairs) <- core waves edgeW series start end =
      VU.toList (VU.drop n (synth (n + predict) mu pairs))
  | otherwise = []

core :: Int -> Double -> [Double] -> Int -> Int -> Maybe (Int, Double, [(Double, (Double, Double))])
core waves edgeW series start end
  | n < minWindow = Nothing
  | otherwise = Just (n, mu, pairs)
  where
    len = length series
    lo = clamp 0 (len - 1) start
    hi = clamp (lo + 1) len end
    n = hi - lo
    vys = VU.fromList (take n (drop lo series))
    mu = VU.sum vys / fromIntegral n
    vysC = VU.map (subtract mu) vys
    freqs = topFreqs n vysC (resolveWaves waves n)
    pairs = zip freqs (pairUp (solveLS n edgeW (VU.toList vysC) freqs))

-- walk-forward parameter search: for every (window, waves) candidate,
-- roll the forecast origin across the series and score each anchor by
-- the hybrid objective
--   score = w * relativeRMSE + (1 - w) * (1 - directionAccuracy)
-- where w = rmsePct / 100. Anchors run up to 50 candles before the tail
-- so the evaluated forecast always overlaps real data; a longer forecast
-- is scored on its overlapping head only. Returns (bestWindow,
-- bestWaves, bestScore); (0, 0, 0) when no candidate has a valid anchor
fftBest :: Int -> Int -> [Double] -> (Int, Int, Double)
fftBest rmsePct predict closes
  | null scored = (0, 0, 0)
  | otherwise = let (s, w, k) = minimumBy (\(a, _, _) (b, _, _) -> compare a b) scored
                in (w, k, s)
  where
    len = length closes
    vcloses = VU.fromList closes
    w = clamp 0 1 (fromIntegral rmsePct / 100)
    cands = bestGrid len predict
    scored = [ (s, win, k)
             | (win, k) <- cands
             , Just s <- [score w predict vcloses (win, k)]
             ]

bestGrid :: Int -> Int -> [(Int, Int)]
bestGrid len predict =
  [ (win, k)
  | win <- dedup [ clamp minWindow (len - minOv) (round (f * fromIntegral predict)) | f <- [2, 3, 4.5, 7, 11, 17] ]
  , k <- dedup [ clamp 1 (max 1 (win `div` 8)) kw | kw <- [2, 4, 8, 16, 24, 32] ]
  ]
  where
    minOv = min 50 predict

score :: Double -> Int -> VU.Vector Double -> (Int, Int) -> Maybe Double
score w predict vcloses (win, waves) =
  let len = VU.length vcloses
      step = max 1 predict
      minOv = min 50 predict
      anchors = [win, win + step .. len - minOv]
      one a =
        let avail = min predict (len - a)
            fc = take avail (fftForecast waves 1 (VU.toList (VU.slice (a - win) win vcloses)) 0 win predict)
            act = VU.toList (VU.slice a avail vcloses)
            scale = max 1e-9 (abs (head act))
            rmse = sqrt (sum [ (p - x) ^ 2 | (p, x) <- zip fc act ] / fromIntegral avail) / scale
        in w * rmse + (1 - w) * (1 - dirAcc fc act)
  in if null anchors
       then Nothing
       else Just (sum (map one anchors) / fromIntegral (length anchors))

-- fraction of candles where the predicted move direction matches the
-- actual one (zero moves excluded)
dirAcc :: [Double] -> [Double] -> Double
dirAcc fc act =
  let sgn x = if x > 0 then 1 :: Int else if x < 0 then -1 else 0
      pairs = zip (zip fc (tail fc)) (zip act (tail act))
      good = length [ () | ((a, b), (x, y)) <- pairs, sgn (b - a) /= 0, sgn (b - a) == sgn (y - x) ]
      tot = length [ () | ((a, b), (x, y)) <- pairs, sgn (b - a) /= 0, sgn (y - x) /= 0 ]
  in if tot == 0 then 0.5 else fromIntegral good / fromIntegral tot

dedup :: Eq a => [a] -> [a]
dedup = foldl' add []
  where
    add seen x
      | x `elem` seen = seen
      | otherwise = seen ++ [x]

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
corrV vys w = corrWV (VU.length vys) vys 1 w

-- weighted variant: edgeW > 1 scales normalized ramps at both ends (the
-- bump mass equals the window length, so edgeW reads as "one edge candle
-- counts as ~edgeW bulk candles")
corrWV :: Int -> VU.Vector Double -> Double -> Double -> (Double, Double)
corrWV n vys edgeW w = go 0 1 0 0 0
  where
    cw = cos w
    sw = sin w
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    wt i =
      let li = 1 - fromIntegral i / fromIntegral h
          ri = 1 - fromIntegral (n - 1 - i) / fromIntegral h
      in 1 + a * max 0 (max li ri)
    go !i !c !s !cc !ss
      | i >= n = (cc, ss)
      | otherwise = go (i + 1) (c * cw - s * sw) (s * cw + c * sw) (cc + wt i * vys VU.! i * c) (ss + wt i * vys VU.! i * s)

-- Gram matrix of the cos/sin design columns in closed form. The base
-- case is the Dirichlet kernel; edgeW > 1 adds linear ramps over n/8
-- candles at both window edges (weights 1 + (edgeW-1) * ramp), keeping
-- every entry an O(1) closed form via geometric and
-- arithmetico-geometric sums. Frequencies are assumed on exact window
-- DFT bins in production, but the kernels are phase-wrapped and total.
gram :: Int -> [Double] -> [[Double]]
gram n ws = gramW n 1 ws

gramW :: Int -> Double -> [Double] -> [[Double]]
gramW n edgeW ws = [ [ entry i j | j <- [0 .. m - 1] ] | i <- [0 .. m - 1] ]
  where
    m = 2 * length ws
    n' = fromIntegral n
    -- almost point weights at both edges (a candle or two of falloff):
    -- leverage at the boundary is 2*k*w0/sum(w), so with a rich basis
    -- (high wave count) even moderate edgeW pins the endpoints, while a
    -- wide ramp only spreads the weight over candles the smooth basis
    -- cannot match anyway
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    w i = ws !! (i `div` 2)
    entry i j
      | i == j = if even i then cc 0 / 2 + cc (2 * wi) / 2 else cc 0 / 2 - cc (2 * wi) / 2
      | even i && even j = (cc (wi - wj) + cc (wi + wj)) / 2
      | odd i && odd j = (cc (wi - wj) - cc (wi + wj)) / 2
      | even i = (ss (wi + wj) + ss (wj - wi)) / 2
      | otherwise = (ss (wi + wj) + ss (wi - wj)) / 2
      where
        wi = w i
        wj = w j
    -- weighted sums: cc c = sum w_t cos(c t), ss c = sum w_t sin(c t);
    -- the ramp sums are evaluated at the phase wrapped to (-pi, pi], so
    -- multiples of 2*pi land on the c≈0 path instead of the sin(c/2) pole
    cc c = d c + a * rampReal (wrap c)
    ss c = s c + a * rampImag (wrap c)
    d x0
      | abs x < 1e-12 = n'
      | otherwise = sin (n' * x / 2) * cos ((n' - 1) * x / 2) / sin (x / 2)
      where
        x = wrap x0
    s x0
      | abs x < 1e-12 = 0
      | otherwise = sin (n' * x / 2) * sin ((n' - 1) * x / 2) / sin (x / 2)
      where
        x = wrap x0
    -- reduce to (-pi, pi] so multiples of 2*pi hit the guards instead of
    -- the sin(x/2) pole
    wrap x = x - 2 * pi * fromIntegral (round (x / (2 * pi)))
    rampReal c = realPart lc + realPart (mirror c lc)
      where
        lc = leftRamp h n c
    rampImag c = imagPart lc + imagPart (mirror c lc)
      where
        lc = leftRamp h n c
    -- right ramp = left ramp mirrored about the last candle
    mirror c lc = cis (c * fromIntegral (n - 1)) * conjugate lc

-- total mass of one linear ramp (1 - t/h on t = 0..m) times two ends
rampMass :: Int -> Int -> Double
rampMass h n = 2 * ((m' + 1) - m' * (m' + 1) / (2 * h'))
  where
    m = min h (n - 1)
    m' = fromIntegral m
    h' = fromIntegral h

-- sum of the left-edge ramp (1 - t/h on t = 0..min h (n-1), else 0)
-- against e^{i c t}
leftRamp :: Int -> Int -> Double -> Complex Double
leftRamp h n c
  | abs c < 1e-12 = fromIntegral (m + 1) - tgeo 0 m / fromIntegral h
  | otherwise = geo - tgeo c m / fromIntegral h
  where
    m = min h (n - 1)
    geo = cis (c * fromIntegral m / 2) * ((sin (fromIntegral (m + 1) * c / 2) / sin (c / 2)) :+ 0)

-- sum_{t=0}^m t e^{i c t}
tgeo :: Double -> Int -> Complex Double
tgeo c m
  | abs c < 1e-12 = fromIntegral (m * (m + 1)) / 2
  | otherwise = z * (1 - fromIntegral (m + 1) * zm + fromIntegral m * zm * z) / (1 - z) ^ 2
  where
    z = cis c
    zm = cis (c * fromIntegral m)

-- least squares fit of cos/sin columns at the given frequencies (cycles
-- per sample) against ys; edgeW > 1 endpoint-weights the fit. The Gram
-- matrix is eigendecomposed (Jacobi) and eigen-directions below 1e-10 of
-- the largest eigenvalue are truncated, so collinear columns (high wave
-- counts, padded FFT bins) cannot produce the huge coefficients that
-- make extrapolation explode
solveLS :: Int -> Double -> [Double] -> [Double] -> [Double]
solveLS n edgeW ys freqs = x
  where
    ws = map (2 * pi *) freqs
    g = gramW n edgeW ws
    vys = VU.fromList ys
    b = concatMap (\w -> let (c, s) = corrWV n vys edgeW w in [c, s]) ws
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
