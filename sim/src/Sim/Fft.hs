{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim.Fft (fftApprox, fftBest, fftForecast, solveLS, gram, gramW, corr, corrWV, rampMass, refinePeaks) where

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
  | otherwise = Just (n, mu + c0, pairs)
  where
    len = length series
    lo = clamp 0 (len - 1) start
    hi = clamp (lo + 1) len end
    n = hi - lo
    vys = VU.fromList (take n (drop lo series))
    mu = VU.sum vys / fromIntegral n
    vysC = VU.map (subtract mu) vys
    k = resolveWaves waves n
    peaks = topPeaks n vysC k
    snapped = map fst peaks
    refined = if k <= 16 then refinePeaks n vysC peaks else snapped
    -- refined frequencies must win by a margin: a near-bin refinement
    -- only absorbs harmonic leakage (small relative gain), while a
    -- genuinely misplaced snap gains orders of magnitude -- so the
    -- margin keeps exact-bin projections orthogonal without blocking
    -- real corrections
    freqs = if 10 * sseOf refined < 9 * sseOf snapped then refined else snapped
    ys = VU.toList vysC
    xf = solveLSI n edgeW ys freqs
    (c0, pairs) = case xf of
      (c : rest) -> (c, zip freqs (pairUp rest))
      [] -> (0, [])
    yy = VU.sum (VU.map (\v -> v * v) vysC)
    sseOf fs =
      let (g, b, x') = solveGBI n 1 ys fs
      in yy - 2 * dotL b x' + quadL g x'

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
-- to exact window DFT bins (f = q / n); the snapped cos/sin columns are
-- exactly orthogonal over the window, so the least squares solve cannot
-- produce runaway coefficients. Returns the snapped frequency paired
-- with the zero-padded peak position (j / m, twice the resolution --
-- snapping round(j * n / m) can land a full bin off the true peak when
-- the padded peak sits near a bin edge). Picks are non-maximum
-- suppressed within +-m/n padded bins so one strong wave's mainlobe
-- straddle cannot starve weaker harmonics of the top-k slots. A hard
-- low-pass drops bins with period under 8 candles: on candle data the
-- upper spectrum is per-candle noise, and fitting it makes the forecast
-- jitter instead of continuing the visible structure (iterative in-place
-- unboxed FFT)
topPeaks :: Int -> VU.Vector Double -> Int -> [(Double, Double)]
topPeaks n vys k =
  let m = nextPow2 n
      spec = fftV m vys
      bins = [ (j, magnitude (spec VU.! j)) | j <- [1 .. m `div` 2] ]
      ranked = sortOn (negate . snd) bins
      chosen = pick [] [] ranked
      snapped = dedup
        [ (round (fromIntegral j * fromIntegral n / fromIntegral m), fp)
        | (j, fp) <- chosen ]
  in [ (fromIntegral q / fromIntegral n, fp)
     | (q, fp) <- snapped, q > 0, 2 * q < n, q <= n `div` 8 ]
  where
    rad = max 1 ((nextPow2 n + n - 1) `div` n)
    pick _ acc [] = reverse acc
    pick seen acc ((j, _) : rest)
      | length acc >= k = reverse acc
      | any (\t -> abs (t - j) <= rad) seen = pick seen acc rest
      | otherwise = pick (j : seen) ((j, fromIntegral j / fromIntegral (nextPow2 n)) : acc) rest
    dedup = foldl' add []
    add seen p@(q, _)
      | any ((== q) . fst) seen = seen
      | otherwise = seen ++ [p]

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

-- correlations of the intercept-extended design [1, cos/sin pairs]: the
-- head is the weighted sum of ys
corrWI :: Int -> VU.Vector Double -> Double -> [Double] -> [Double]
corrWI n vys edgeW ws = wsumY : concatMap (\w -> let (c, s) = corrWV n vys edgeW w in [c, s]) ws
  where
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    wt i =
      let li = 1 - fromIntegral i / fromIntegral h
          ri = 1 - fromIntegral (n - 1 - i) / fromIntegral h
      in 1 + a * max 0 (max li ri)
    wsumY = go 0 0
      where
        go !i !acc
          | i >= n = acc
          | otherwise = go (i + 1) (acc + wt i * vys VU.! i)

-- refine the strongest detected bins by local energy maximization near
-- the zero-padded peak position (within one window bin: the padded
-- estimate is already within half a padded bin of the true frequency,
-- and the snapped grid can sit a full bin off). The snapped grid is 1/n
-- coarse, so a signal whose period does not divide the window lands up
-- to half a bin off its true frequency and the fitted curve drifts out
-- of phase toward the window edges (worst at frac(n/p) ~ 0.5). The top
-- two bins are refined sequentially against the residual of the
-- already-refined component; further bins stay snapped. A refined value
-- colliding with another bin (closer than half a window bin) falls back
-- to the snapped grid, keeping the near-orthogonal safety of the solve
-- intact. Callers pick between snapped and refined by in-window SSE, so
-- a noise-displaced refinement can never beat an exact snapped bin.
refinePeaks :: Int -> VU.Vector Double -> [(Double, Double)] -> [Double]
refinePeaks _ _ [] = []
refinePeaks n vys peaks = go vys [] peaks
  where
    half = 0.5 / fromIntegral n
    go _ done ps
      | length done >= 2 = map fst ps
    go resid done ((fs, fp) : rest) =
      let cand = refineOne n resid fp
          f = if any (\h -> abs (cand - h) < half) (done ++ map fst rest) then fs else cand
      in f : go (subtractComp n resid f) (done ++ [f]) rest
    go _ _ [] = []

-- peak of the single-component LS energy within one window bin of the
-- zero-padded peak position f0 (ternary search; the Dirichlet mainlobe
-- is unimodal there). The objective is the explained energy of the
-- sinusoid part under the intercept model -- centering the columns
-- against the constant -- not the plain correlation energy c^2+s^2:
-- off the exact bins the cos/sin columns are neither orthogonal nor
-- equal-norm, and the raw-energy peak is displaced by up to ~0.1 bin at
-- low frequencies
refineOne :: Int -> VU.Vector Double -> Double -> Double
refineOne n vys f0 = go (f0 - rad) (f0 + rad) 80
  where
    rad = 1 / fromIntegral n
    ones = VU.replicate n 1
    n' = fromIntegral n
    e f =
      let w = 2 * pi * f
          (c1, s1) = corrV ones w
          (rc, rs) = corrV ones (2 * w)
          (bc, bs) = corrV vys w
          sy = VU.sum vys
          g11 = (n' + rc) / 2 - c1 * c1 / n'
          g22 = (n' - rc) / 2 - s1 * s1 / n'
          g12 = rs / 2 - c1 * s1 / n'
          bc' = bc - c1 * sy / n'
          bs' = bs - s1 * sy / n'
          det = g11 * g22 - g12 * g12
      in (bc' * bc' * g22 + bs' * bs' * g11 - 2 * bc' * bs' * g12) / det
    go a b it
      | it <= (0 :: Int) = (a + b) / 2
      | otherwise =
          let m1 = a + (b - a) / 3
              m2 = b - (b - a) / 3
          in if e m1 < e m2 then go m1 b (it - 1) else go a m2 (it - 1)

-- remove the LS-fitted sinusoid part at frequency f from vys under the
-- intercept model (centered 2x2 closed forms); the fitted intercept is
-- left in the residual -- it is the shared DC, the next component's
-- refinement handles it via the centered objective
subtractComp :: Int -> VU.Vector Double -> Double -> VU.Vector Double
subtractComp n vys f = VU.imap (\i y -> y - a * cos (w * t i) - b * sin (w * t i)) vys
  where
    w = 2 * pi * f
    t i = fromIntegral i
    ones = VU.replicate n 1
    n' = fromIntegral n
    (c1, s1) = corrV ones w
    (rc, rs) = corrV ones (2 * w)
    sy = VU.sum vys
    g11 = (n' + rc) / 2 - c1 * c1 / n'
    g22 = (n' - rc) / 2 - s1 * s1 / n'
    g12 = rs / 2 - c1 * s1 / n'
    (bc, bs) = corrV vys w
    bc' = bc - c1 * sy / n'
    bs' = bs - s1 * sy / n'
    det = g11 * g22 - g12 * g12
    a = (g22 * bc' - g12 * bs') / det
    b = (g11 * bs' - g12 * bc') / det

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
-- weighted sums sum w_t cos(c t) / sum w_t sin(c t) over t = 0..n-1
-- with weights 1 + (edgeW-1) * edge ramps (closed forms via the
-- Dirichlet kernel and geometric / arithmetico-geometric sums, phases
-- wrapped so multiples of 2*pi hit the guards instead of the sin(c/2)
-- pole)
wCos :: Int -> Double -> Double -> Double
wCos n edgeW c0 = base + a * rampReal n h c
  where
    c = wrap c0
    n' = fromIntegral n
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    base
      | abs c < 1e-12 = n'
      | otherwise = sin (n' * c / 2) * cos ((n' - 1) * c / 2) / sin (c / 2)

wSin :: Int -> Double -> Double -> Double
wSin n edgeW c0 = base + a * rampImag n h c
  where
    c = wrap c0
    n' = fromIntegral n
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    base
      | abs c < 1e-12 = 0
      | otherwise = sin (n' * c / 2) * sin ((n' - 1) * c / 2) / sin (c / 2)

-- reduce to (-pi, pi] so multiples of 2*pi hit the guards instead of
-- the sin(x/2) pole
wrap :: Double -> Double
wrap x = x - 2 * pi * fromIntegral (round (x / (2 * pi)))

rampReal :: Int -> Int -> Double -> Double
rampReal n h c = realPart lc + realPart (mirror n c lc)
  where
    lc = leftRamp h n c

rampImag :: Int -> Int -> Double -> Double
rampImag n h c = imagPart lc + imagPart (mirror n c lc)
  where
    lc = leftRamp h n c

-- right ramp = left ramp mirrored about the last candle
mirror :: Int -> Double -> Complex Double -> Complex Double
mirror n c lc = cis (c * fromIntegral (n - 1)) * conjugate lc

gram :: Int -> [Double] -> [[Double]]
gram n ws = gramW n 1 ws

gramW :: Int -> Double -> [Double] -> [[Double]]
gramW n edgeW ws = [ [ entry i j | j <- [0 .. m - 1] ] | i <- [0 .. m - 1] ]
  where
    m = 2 * length ws
    w i = ws !! (i `div` 2)
    cc c = wCos n edgeW c
    ss c = wSin n edgeW c
    entry i j
      | i == j = if even i then cc 0 / 2 + cc (2 * wi) / 2 else cc 0 / 2 - cc (2 * wi) / 2
      | even i && even j = (cc (wi - wj) + cc (wi + wj)) / 2
      | odd i && odd j = (cc (wi - wj) - cc (wi + wj)) / 2
      | even i = (ss (wi + wj) + ss (wj - wi)) / 2
      | otherwise = (ss (wi + wj) + ss (wi - wj)) / 2
      where
        wi = w i
        wj = w j

-- Gram of the intercept-extended design [1, cos/sin pairs]: the first
-- row/column are the constant's sums (total weight, weighted cos/sin
-- sums); the cos/sin block is gramW. Demeaning alone cannot replace the
-- intercept -- the window mean is not in the span of the cos/sin
-- columns, and without a constant column the low-frequency fits
-- compromise toward the leftover DC instead of the true frequency
gramWI :: Int -> Double -> [Double] -> [[Double]]
gramWI n edgeW ws =
  (constSum : [ colSum j | j <- [0 .. m - 1] ]) : [ (colSum i : row) | (i, row) <- zip [0 ..] g ]
  where
    m = 2 * length ws
    g = gramW n edgeW ws
    h = max 2 (n `div` 128)
    a = max 0 (edgeW - 1)
    constSum = fromIntegral n + a * rampMass h n
    colSum j
      | even j = wCos n edgeW (ws !! (j `div` 2))
      | otherwise = wSin n edgeW (ws !! (j `div` 2))

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
      | l > 1e-10 * lmax = dotL v b * l / (l * l + lam)
      | otherwise = 0
    x = [ sum [ v !! i * coef (l, v) | (l, v) <- zip evals evecs ] | i <- [0 .. length g - 1] ]

-- intercept-extended front end: coefficients [c0, a0, b0, ...]
solveLSI :: Int -> Double -> [Double] -> [Double] -> [Double]
solveLSI n edgeW ys freqs = x
  where
    (_, _, x) = solveGBI n edgeW ys freqs

-- the intercept-extended fit: like solveGB but with a constant column
-- first, so the coefficients are [c0, a0, b0, a1, b1, ...]
solveGBI :: Int -> Double -> [Double] -> [Double] -> ([[Double]], [Double], [Double])
solveGBI n edgeW ys freqs = (g, b, x)
  where
    ws = map (2 * pi *) freqs
    g = gramWI n edgeW ws
    vys = VU.fromList ys
    b = corrWI n vys edgeW ws
    (evals, evecs) = jacobi g
    lmax = maximum (map abs evals)
    lam = (1e-6 * lmax) ^ 2
    coef (l, v)
      | l > 1e-10 * lmax = dotL v b * l / (l * l + lam)
      | otherwise = 0
    x = [ sum [ v !! i * coef (l, v) | (l, v) <- zip evals evecs ] | i <- [0 .. length g - 1] ]

dotL :: [Double] -> [Double] -> Double
dotL a c = sum (zipWith (*) a c)

quadL :: [[Double]] -> [Double] -> Double
quadL g x = sum [ dotL (g !! i) x * (x !! i) | i <- [0 .. length x - 1] ]

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
