{-# LANGUAGE BangPatterns #-}

-- classic TA indicators over close prices: SMA, EMA, MACD, RSI,
-- Bollinger bands. All series have one output per input candle with NaN
-- warm-up where the window is not full (same convention as
-- Sim.Strat.Backtest.smaSeries). Pure math only — the chart widget
-- (indicator_widget.gd) calls these through the FFI in SimFFI.
module Sim.Ind
  ( smaSeries
  , emaSeries
  , macdSeries
  , rsiSeries
  , bollingerSeries
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

nan :: Double
nan = 0 / 0

-- running SMA; NaN while the window is not full (i < p - 1)
smaSeries :: Int -> VU.Vector Double -> VU.Vector Double
smaSeries p v
  | p < 1 || VU.null v = VU.empty
  | otherwise =
      let sc = VU.postscanl' (+) 0 v
          n = VU.length v
       in VU.generate n $ \i ->
            if i < p - 1
              then nan
              else (sc VU.! i - (if i >= p then sc VU.! (i - p) else 0)) / fromIntegral p

-- exponential moving average, TA-Lib seeding: ema[p-1] is the SMA of the
-- first p values, then ema[i] = k*x[i] + (1-k)*ema[i-1] with
-- k = 2/(p+1). NaN while i < p - 1.
emaSeries :: Int -> VU.Vector Double -> VU.Vector Double
emaSeries p v
  | p < 1 || VU.null v = VU.empty
  | n < p = VU.replicate n nan
  | otherwise = runST $ do
      out <- VUM.new n
      forM_ [0 .. p - 2] $ \i -> VUM.write out i nan
      VUM.write out (p - 1) seed
      let go !i !prev
            | i >= n = pure ()
            | otherwise = do
                let e = k * (v VU.! i) + (1.0 - k) * prev
                VUM.write out i e
                go (i + 1) e
      go p seed
      VU.unsafeFreeze out
  where
    n = VU.length v
    k = 2.0 / (fromIntegral p + 1.0)
    seed = VU.sum (VU.slice 0 p v) / fromIntegral p

-- MACD: macd = emaFast - emaSlow, signal = EMA(sig) of the macd line
-- (seeded over the macd's valid suffix, TA-Lib style), hist = macd -
-- signal. NaN warm-up: macd for i < slow-1, signal for
-- i < slow-1 + sig-1.
macdSeries :: Int -> Int -> Int -> VU.Vector Double -> VU.Vector (Double, Double, Double)
macdSeries fast slow sig v
  | fast < 1 || slow < 1 || sig < 1 || VU.null v = VU.empty
  | otherwise = VU.zip3 macd sigLine hist
  where
    macd = VU.zipWith (-) (emaSeries fast v) (emaSeries slow v)
    -- the signal EMA starts once the macd line is valid (index slow-1);
    -- its own warm-up adds sig-1 more NaNs; take keeps the length exact
    -- when the input is shorter than slow-1
    sigLine =
      VU.take (VU.length macd) $
        VU.concat [VU.replicate (slow - 1) nan, emaSeries sig (VU.drop (slow - 1) macd)]
    hist = VU.zipWith (-) macd sigLine

-- Wilder's RSI over close-to-close changes: first value at index n uses
-- simple averages of the first n changes, then the Wilder recursion
-- avg = (avg*(n-1) + x)/n. RSI = 100 - 100/(1 + avgG/avgL); avgL == 0
-- gives 100, a flat series (both zero) gives 50. NaN for i < n.
rsiSeries :: Int -> VU.Vector Double -> VU.Vector Double
rsiSeries n0 v
  | n < 1 || len < n + 1 = VU.replicate len nan
  | otherwise = runST $ do
      out <- VUM.new len
      forM_ [0 .. n - 1] $ \i -> VUM.write out i nan
      VUM.write out n (rsiOf g0 l0)
      let go !i !g !l
            | i >= len = pure ()
            | otherwise = do
                let g' = (g * fromIntegral (n - 1) + gains VU.! (i - 1)) / fromIntegral n
                    l' = (l * fromIntegral (n - 1) + losses VU.! (i - 1)) / fromIntegral n
                VUM.write out i (rsiOf g' l')
                go (i + 1) g' l'
      go (n + 1) g0 l0
      VU.unsafeFreeze out
  where
    n = max 1 n0
    len = VU.length v
    diffs = VU.generate (len - 1) (\i -> v VU.! (i + 1) - v VU.! i)
    gains = VU.map (max 0.0) diffs
    losses = VU.map (max 0.0 . negate) diffs
    g0 = VU.sum (VU.slice 0 n gains) / fromIntegral n
    l0 = VU.sum (VU.slice 0 n losses) / fromIntegral n
    rsiOf g l
      | l == 0.0 = if g == 0.0 then 50.0 else 100.0
      | otherwise = 100.0 - 100.0 / (1.0 + g / l)

-- Bollinger bands: mid = SMA(p), band = mid +/- mult * population std
-- (dividing by p, like TA-Lib), NaN while the window is not full
bollingerSeries :: Int -> Double -> VU.Vector Double -> VU.Vector (Double, Double, Double)
bollingerSeries p0 mult v
  | p < 1 || VU.null v = VU.empty
  | n < p = VU.replicate n (nan, nan, nan)
  | otherwise = VU.generate n $ \i ->
      if i < p - 1
        then (nan, nan, nan)
        else
          let m = mid VU.! i
              var = VU.sum (VU.map (\x -> (x - m) * (x - m)) (VU.slice (i - p + 1) p v)) / fromIntegral p
              s = sqrt var
           in (m, m + mult * s, m - mult * s)
  where
    p = max 1 p0
    n = VU.length v
    mid = smaSeries p v
