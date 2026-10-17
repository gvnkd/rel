{-# LANGUAGE StrictData #-}

-- candle-close backtest for SMA-cross strategies. Signal per candle comes
-- from Sim.Strat.Rules+evalSignal; a position held over i -> i+1 earns
-- pos * (close i+1 / close i - 1) minus fee * |position change| — the
-- same accounting the ML trainer uses.
module Sim.Strat.Backtest
  ( smaSeries
  , cmpD
  , crossEvents
  , evalSignal
  , BtMetrics (..)
  , BacktestResult (..)
  , backtest
  ) where

import Data.List (sortOn)
import qualified Data.Vector.Unboxed as VU
import Sim.Strat.Rules (Strat (..), Slot (..))

-- running SMA; NaN while the window is not full (i < w - 1)
smaSeries :: Int -> VU.Vector Double -> VU.Vector Double
smaSeries w c
  | w <= 0 || VU.null c = VU.empty
  | otherwise =
      let sc = VU.postscanl' (+) 0 c
          n = VU.length c
      in VU.generate n $ \i ->
           if i < w - 1
             then 0 / 0
             else (sc VU.! i - (if i >= w then sc VU.! (i - w) else 0)) / fromIntegral w

-- NaN and exact zeros count as "no side" for sign comparisons
cmpD :: Double -> Ordering
cmpD x | isNaN x = EQ
       | x > 0   = GT
       | x < 0   = LT
       | otherwise = EQ

-- NaN marks the SMA warm-up. A cross fires when the diff leaves NaN with
-- a definite side (the slot "activates"), and afterwards on any strict
-- sign change between non-zero values.
crossEvents :: VU.Vector Double -> [Int]
crossEvents d = go 1 []
  where
    n = VU.length d
    go i acc
      | i >= n = reverse acc
      | isNaN (d VU.! (i - 1)) =
          if cmpD (d VU.! i) /= EQ then go (i + 1) (i : acc) else go (i + 1) acc
      | cmpD (d VU.! (i - 1)) /= EQ
        && cmpD (d VU.! i) /= EQ
        && cmpD (d VU.! (i - 1)) /= cmpD (d VU.! i) = go (i + 1) (i : acc)
      | otherwise = go (i + 1) acc

-- per-candle position: the position of the slot whose (fast, slow) pair
-- crossed most recently at or before this candle; 0 before the first
-- cross. Same-index crosses: the later slot in the list wins.
evalSignal :: Strat -> VU.Vector Double -> VU.Vector Int
evalSignal st closes
  | VU.null closes = VU.empty
  | otherwise =
      let evs = sortOn fst
            [ (i, slPos s)
            | s <- stSlots st
            , i <- crossEvents (slotDiff s) ]
      in VU.fromList (walk 0 evs [0 .. VU.length closes - 1])
  where
    slotDiff s = VU.zipWith (-) (smaSeries (slFast s) closes) (smaSeries (slSlow s) closes)
    walk pos evs is = case (evs, is) of
      ([], _ : rest)       -> pos : walk pos [] rest
      ((j, p) : es, i : rest)
        | j <= i           -> walk p es (i : rest)
        | otherwise        -> pos : walk pos evs rest
      _                    -> []

data BtMetrics = BtMetrics
  { bmPnl    :: !Double
  , bmMaxDD  :: !Double
  , bmCalmar :: !Double
  , bmTrades :: !Int
  } deriving (Show)

data BacktestResult = BacktestResult
  { brMetrics :: !BtMetrics
  , brEquity  :: !(VU.Vector Double)
  , brPos     :: !(VU.Vector Int)     -- position per step
  , brCloses  :: !(VU.Vector Double)  -- entry close per step
  } deriving (Show)

-- walk i in [lo, hi): position = signal at i, marked over i -> i+1,
-- starting flat. The caller guarantees hi < n so i+1 is valid.
backtest :: Double -> VU.Vector Double -> VU.Vector Int -> (Int, Int) -> BacktestResult
backtest fee closes sig (lo, hi) =
  let go prev (eq, peak, dd, pnl, trades, eqs, poss, cls) i
        | i >= hi =
            BacktestResult (BtMetrics pnl dd (if dd > 1e-12 then pnl / dd else pnl) trades)
                           (VU.fromList (reverse eqs)) (VU.fromList (reverse poss))
                           (VU.fromList (reverse cls))
        | otherwise =
            let pos = sig VU.! i
                c = closes VU.! i
                r = closes VU.! (i + 1) / c - 1
                step = fromIntegral pos * r - fee * fromIntegral (abs (pos - prev))
                eq' = eq + step
                peak' = max peak eq'
                dd' = max dd (peak' - eq')
            in go pos (eq', peak', dd', pnl + step, trades + (if pos /= prev then 1 else 0),
                       eq' : eqs, pos : poss, c : cls) (i + 1)
  in if VU.null closes || lo >= hi then
       BacktestResult (BtMetrics 0 0 0 0) VU.empty VU.empty VU.empty
     else go 0 (0, 0, 0, 0, 0, [], [], []) lo
