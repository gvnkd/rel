{-# LANGUAGE StrictData #-}

-- candle-close backtest for SMA-cross strategies. Signal per candle comes
-- from Sim.Strat.Rules+evalSignal; a position held over i -> i+1 earns
-- pos * (close i+1 / close i - 1) minus fee * |position change| — the
-- same accounting the ML trainer uses.
--
-- backtestWithTrades additionally reconstructs the round-trip trade list
-- (entry at close[i0], exit at the candle where the position changes)
-- with the cause carried by the signal evaluator (which rule/slot fired).
module Sim.Strat.Backtest
  ( smaSeries
  , cmpD
  , crossEvents
  , evalSignal
  , evalSignalC
  , BtMetrics (..)
  , BacktestResult (..)
  , backtest
  , Trade (..)
  , backtestWithTrades
  ) where

import Data.List (sortOn)
import qualified Data.Vector as V
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
evalSignal st closes = fst (evalSignalC st closes)

-- signal + per-candle cause ("slot f:s:p" of the winning slot)
evalSignalC :: Strat -> VU.Vector Double -> (VU.Vector Int, V.Vector String)
evalSignalC st closes
  | VU.null closes = (VU.empty, V.empty)
  | otherwise =
      let evs = sortOn fst3
            [ (i, slPos s, slotCause s)
            | s <- stSlots st
            , i <- crossEvents (slotDiff s) ]
      in ( VU.fromList (walk 0 evs [0 .. VU.length closes - 1])
         , V.fromList (walkC "" evs [0 .. VU.length closes - 1]) )
  where
    fst3 (a, _, _) = a
    slotCause s = "slot " ++ show (slFast s) ++ ":" ++ show (slSlow s)
                  ++ ":" ++ show (slPos s)
    slotDiff s = VU.zipWith (-) (smaSeries (slFast s) closes) (smaSeries (slSlow s) closes)
    walk pos evs is = case (evs, is) of
      ([], _ : rest)          -> pos : walk pos [] rest
      ((j, p, _) : es, i : rest)
        | j <= i              -> walk p es (i : rest)
        | otherwise           -> pos : walk pos evs rest
      _                       -> []
    walkC cause evs is = case (evs, is) of
      ([], _ : rest)          -> cause : walkC cause [] rest
      ((j, _, c) : es, i : rest)
        | j <= i              -> walkC c es (i : rest)
        | otherwise           -> cause : walkC cause evs rest
      _                       -> []

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

-- one round trip: opened at close[i0] when the position took `side`,
-- closed at close[i1] when the position next changed (i1 is the candle
-- where the exit fills, so consecutive trades tile [lo, hi) exactly).
-- trPnl = side * (exit/entry - 1) - 2 * fee (one fee in, one fee out).
-- trVol is the bar volume at entry, supplied by the caller.
data Trade = Trade
  { trI0    :: !Int
  , trI1    :: !Int
  , trSide  :: !Int
  , trCause :: !String
  , trEntry :: !Double
  , trExit  :: !Double
  , trPnl   :: !Double
  , trVol   :: !Double
  } deriving (Eq, Show)

-- backtest + trade reconstruction. A position still open at hi-1 is
-- closed synthetically at close[hi]. The trade list tiles the whole
-- walked range: the first trade opens at the first candle with a
-- non-flat signal, and each subsequent trade opens where the previous
-- closed.
backtestWithTrades :: Double -> VU.Vector Double -> VU.Vector Double
                   -> VU.Vector Int -> V.Vector String
                   -> (Int, Int) -> (BacktestResult, [Trade])
backtestWithTrades fee closes vols sig causes (lo, hi)
  | VU.null closes || lo >= hi = (backtest fee closes sig (lo, hi), [])
  | otherwise =
      let bt = backtest fee closes sig (lo, hi)
          n = VU.length closes
          hiC = min hi (n - 1)  -- close[hiC] is the last valid exit price
          go i prev open acc
            | i >= hiC =
                case open of
                  Just (i0, side, entry, cause)
                    | prev /= 0 ->
                        let exit = closes VU.! hiC
                            v = if VU.null vols then 0 else vols VU.! i0
                        in reverse (Trade i0 hiC side cause entry exit
                                      (sideD side entry exit - 2 * fee) v : acc)
                  _ -> reverse acc
            | otherwise =
                let pos = sig VU.! i
                in if pos == prev
                     then go (i + 1) prev open acc
                     else
                       let closed = case open of
                             Just (i0, side, entry, cause)
                               | prev /= 0 ->
                                   let exit = closes VU.! i
                                       v = if VU.null vols then 0 else vols VU.! i0
                                   in Trade i0 i side cause entry exit
                                            (sideD side entry exit - 2 * fee) v : acc
                             _ -> acc
                           opened = if pos /= 0
                             then Just (i, pos, closes VU.! i, causes V.! i)
                             else Nothing
                       in go (i + 1) pos opened closed
          sideD side entry exit =
            fromIntegral side * (exit / entry - 1)
      in (bt, go lo 0 Nothing [])
