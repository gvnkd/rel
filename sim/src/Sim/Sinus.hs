{-# LANGUAGE StrictData #-}

-- pure sinus generator: a closed-form candle source with a known ground
-- truth, for validating indicators (FFT/KS/SMA) against an exact harmonic
-- signal. Replaces the order-book flow entirely -- no book, no RNG.
--
-- The signal is a harmonic ladder: wave k (0-based) has period
-- max 2 (period div (k+1)) and amplitude amp * 0.5^k, so the total swing
-- is bounded by 2*amp and keeping amp at or below a quarter of the base
-- price guarantees a strictly positive range (the 1-tick floor below is
-- the belt-and-suspenders version of the same bound).
--
-- Volumes follow the same ladder a quarter cycle ahead (phase pi/2).
module Sim.Sinus
  ( runSimSinus
  , runSimSinusH
  , sinusCandlesFrom
  , sinusCloseAt
  , sinusPriceAt
  , sinusVolAt
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Sim.Internal

data SinusCfg = SinusCfg
  { scBase   :: !Double     -- start mid, ticks
  , scAmp    :: !Double     -- wave-0 amplitude, ticks (<= base/4)
  , scPeriod :: !Int        -- wave-0 period, candles
  , scWaves  :: !Int        -- harmonic count
  , scVolB   :: !Double     -- volume base
  , scVolA   :: !Double     -- wave-0 volume amplitude
  }

sinusCfg :: SimParams -> SinusCfg
sinusCfg sp = SinusCfg
  { scBase   = base
  , scAmp    = min (base / 4) (max 0 (base * fromIntegral (spSinusAmp sp) / 10000))
  , scPeriod = max 2 (spSinusPeriod sp)
  , scWaves  = max 0 (spSinusWaves sp)
  , scVolB   = max 0 (fromIntegral (spSinusVolBase sp))
  , scVolA   = max 0 (fromIntegral (spSinusVolAmp sp))
  }
  where
    base = fromIntegral (unPrice (spStartBid sp) + unPrice (spStartAsk sp)) / 2

-- wave k's period: the base period halved per harmonic, floored at 2
wavePeriod :: SinusCfg -> Int -> Int
wavePeriod cfg k = max 2 (scPeriod cfg `div` (k + 1))

-- composite price (ticks, unquantized) at absolute candle index i
sinusPriceAt :: SimParams -> Int -> Double
sinusPriceAt sp i = scBase cfg + sum [ scAmp cfg * 0.5 ^ k * sin (2 * pi * fromIntegral i / fromIntegral (wavePeriod cfg k))
                                     | k <- [0 .. scWaves cfg - 1] ]
  where
    cfg = sinusCfg sp

-- composite volume at absolute candle index i, a quarter cycle ahead
sinusVolAt :: SimParams -> Int -> Double
sinusVolAt sp i = scVolB cfg + sum [ scVolA cfg * 0.5 ^ k * sin (2 * pi * fromIntegral i / fromIntegral (wavePeriod cfg k) + pi / 2)
                                   | k <- [0 .. scWaves cfg - 1] ]
  where
    cfg = sinusCfg sp

-- candle at absolute index i given the previous candle's close (the open
-- chains closes; candle 0 opens at its own close). High/low carry a small
-- fixed wick around the body so wick-consuming indicators see non-trivial
-- values; the wick is a constant fraction of wave-0's amplitude.
sinusCandleAt :: SinusCfg -> Price -> Int -> Candle
sinusCandleAt cfg prevClose i =
  Candle (Price o) (Price hi) (Price lo) (Price cl) vol
  where
    o   = if unPrice prevClose > 0 then unPrice prevClose else cl
    cl  = max 1 (round (scBase cfg + sum [ scAmp cfg * 0.5 ^ k * sin (phase k) | k <- [0 .. scWaves cfg - 1] ]))
    hi  = max 1 (max o cl + wick)
    lo  = max 1 (min o cl - wick)
    vol = max 1 (round (scVolB cfg + sum [ scVolA cfg * 0.5 ^ k * sin (phase k + pi / 2) | k <- [0 .. scWaves cfg - 1] ]))
    wick = max 1 (round (scAmp cfg / 20))
    phase k = 2 * pi * fromIntegral i / fromIntegral (wavePeriod cfg k)

-- quantized close at absolute index i
sinusCloseAt :: SimParams -> Int -> Price
sinusCloseAt sp i = candleClose (sinusCandleAt (sinusCfg sp) (Price 0) i)

-- candles for absolute indices [from, from+count); the first candle's
-- open chains the close at from-1 so a resume continues seamlessly
sinusCandlesFrom :: SimParams -> Int -> Int -> [Candle]
sinusCandlesFrom sp from count = go from count prev0 []
  where
    cfg = sinusCfg sp
    prev0 = if from > 0 then sinusCloseAt sp (from - 1) else Price 0
    go _ 0 _ acc = reverse acc
    go i k prev acc =
      let c = sinusCandleAt cfg prev i
      in go (i + 1) (k - 1) (candleClose c) (c : acc)

runSimSinus :: SimParams -> [Candle]
runSimSinus sp = sinusCandlesFrom sp 0 (spCandles sp)

-- hooked variant for the FFI/UI path: per-candle progress and a
-- cancellation poll; the sequence is closed-form so resuming is just
-- starting at a later absolute index
runSimSinusH :: SimHooks -> SimParams -> Int -> IO [Candle]
runSimSinusH hooks sp from = go from (spCandles sp) prev0 []
  where
    total = spCandles sp
    cfg = sinusCfg sp
    prev0 = if from > 0 then sinusCloseAt sp (from - 1) else Price 0
    go _ 0 _ acc = pure (reverse acc)
    go i k prev acc = do
      cancelled <- hookCancelled hooks
      when cancelled (throwIO SimCancelled)
      let c = sinusCandleAt cfg prev i
      hookProgress hooks (i - from + 1) total
      go (i + 1) (k - 1) (candleClose c) (c : acc)
