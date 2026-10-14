module Sim.Fft (fftApprox) where

import Data.Complex (Complex ((:+)), cis, magnitude)
import Data.List (foldl', sortOn)

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
  | otherwise = (map value [0 .. n - 1], map value [n .. n + predict - 1])
  where
    len = length series
    lo = clamp 0 (len - 1) start
    hi = clamp (lo + 1) len end
    n = hi - lo
    win = take n (drop lo series)
    mu = sum win / fromIntegral n
    ys = map (subtract mu) win
    freqs = topFreqs n ys (resolveWaves waves n)
    pairs = pairUp (solveLS n ys freqs)
    value t = mu + sum
      [ c * cos (w * t') + s * sin (w * t')
      | (f, (c, s)) <- zip freqs pairs
      , let w = 2 * pi * f
      , let t' = fromIntegral t
      ]

resolveWaves :: Int -> Int -> Int
resolveWaves 0 n = clamp 1 (n `div` 16) 8
resolveWaves w n = clamp 1 (n `div` 4) w

pairUp :: [a] -> [(a, a)]
pairUp (a : b : rest) = (a, b) : pairUp rest
pairUp _ = []

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- top-k spectral bins (by magnitude) of a zero-padded window, as cycles
-- per sample
topFreqs :: Int -> [Double] -> Int -> [Double]
topFreqs n ys k =
  let m = nextPow2 n
      spec = fft (map (:+ 0) ys ++ replicate (m - n) 0)
      bins = zip [1 ..] (map magnitude (tail spec))
      top = take k (sortOn (\(j, mag) -> negate mag) (take (m `div` 2) bins))
  in map (\(j, _) -> fromIntegral j / fromIntegral m) top

nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go p = if p >= n then p else go (p * 2)

fft :: [Complex Double] -> [Complex Double]
fft [x] = [x]
fft xs =
  let n = length xs
      half = n `div` 2
      evens = fft [x | (x, i) <- zip xs [0 ..], even i]
      odds = fft [x | (x, i) <- zip xs [0 ..], odd i]
      tws = [cis (-2 * pi * fromIntegral j / fromIntegral n) | j <- [0 .. half - 1]]
      combine s = zipWith3 (\e o w -> e + s (w * o)) evens odds tws
  in combine id ++ combine negate

-- least squares fit of cos/sin columns at the given frequencies against ys
-- (ridge-regularized normal equations, partial-pivot Gaussian elimination)
solveLS :: Int -> [Double] -> [Double] -> [Double]
solveLS n ys freqs = gauss g rhs
  where
    ts = map fromIntegral [0 .. n - 1]
    basis =
      concat
        [ [\t -> cos (2 * pi * f * t), \t -> sin (2 * pi * f * t)]
        | f <- freqs
        ]
    cols = map (\f -> map f ts) basis
    m = length cols
    lam = 1e-8 * fromIntegral n
    g =
      [ [ (if i == j then lam else 0) + sum (zipWith (*) (cols !! i) (cols !! j))
        | j <- [0 .. m - 1]
        ]
      | i <- [0 .. m - 1]
      ]
    rhs = [sum (zipWith (*) (cols !! i) ys) | i <- [0 .. m - 1]]

gauss :: [[Double]] -> [Double] -> [Double]
gauss a b = backsub (fwd (zipWith (\r bi -> r ++ [bi]) a b) 0)
  where
    n = length a
    fwd mat i
      | i >= n = mat
      | otherwise = fwd mat' (i + 1)
      where
        piv = i + argmax (map (abs . (!! i)) (drop i mat))
        sw = swapAt i piv mat
        prow = sw !! i
        pv = prow !! i
        factor r = zipWith (-) r (map (* (r !! i / pv)) prow)
        mat' = take (i + 1) sw ++ map factor (drop (i + 1) sw)
    backsub mat = foldl' step [] (reverse mat)
      where
        step acc r =
          let i = n - length acc - 1
              known = sum (zipWith (*) (drop (i + 1) (take n r)) acc)
          in ((r !! n - known) / r !! i) : acc

argmax :: Ord b => [b] -> Int
argmax (x : xs) = snd (foldl' step (x, 0 :: Int) (zip [1 ..] xs))
  where
    step (bv, bi) (k, v) = if v > bv then (v, k) else (bv, bi)
argmax [] = 0

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | otherwise =
      [ if k == i then xs !! j else if k == j then xs !! i else xs !! k
      | k <- [0 .. length xs - 1]
      ]
