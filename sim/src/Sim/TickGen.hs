{-# LANGUAGE StrictData #-}

-- tick-mode export: same order flow as Sim.Gen, additionally recording
-- each trade (and, in ModeTickSteps, each non-trading placement) with a
-- snapshot of the book right after the event.
--
-- the rolling helpers below intentionally duplicate Sim.Gen instead of
-- sharing them: exporting the small worker functions from Sim.Gen costs
-- ~40% of runSim throughput (measured 11s -> 16s on 100M orders).
module Sim.TickGen
  ( runSimTicks
  ) where

import Control.Applicative ((<|>))
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import Sim.Engine
import Sim.Gen (GenState (..), initState)
import Sim.Internal
import Sim.Lvl
import System.Random.SplitMix (nextWord64)

-- candles (compacted, same as runSim) plus the tick export:
-- in ModeTickTrades one row per trade, in ModeTickSteps additionally one
-- row per placed order that did not trade; every candle ends with a
-- book-only marker row carrying the exact end-of-candle book.
-- Additionally the level-lifetime export (Sim.Lvl): one (segment, events)
-- pair per continuous (side, price) existence, in close order; each
-- segment's events are contiguous in the flat event list (lvEv0/lvEvN are
-- indices into it). Open segments flush at the end.
-- per-candle tick lists are collected as chunks and concatenated once
-- at the end: `ts ++ reverse ticks` per candle is quadratic overall.
runSimTicks :: SimParams -> ([Candle], [Tick], [(LvlSeg, [LvlEv])], Int)
runSimTicks sp =
  let (cs, tss, pss, tr, n) = go (spCandles sp) 0 (initState sp) tr0 [] [] [] 0
      closed = concat (reverse pss)
      flushed = trCloseAll (max 0 (n - 1)) tr
  in ( reverse cs
     , concat (reverse tss)
     , closed ++ flushed
     , trTotalEv tr + sum (map (length . snd) flushed)
     )
  where
    -- the seed book (start bid/ask) opens its level lifetimes at candle 0
    tr0 = trFromBook 0 0 (gsBook (initState sp))
    go 0 _ _ tr cs tss pss n = (cs, tss, pss, tr, n)
    go k ci gs tr cs tss pss n =
      let (gs', ticks, tr', pairs) = runBatch sp ci gs tr
          c            = gsCandle gs'
          rolled       = gs' { gsCandle = emptyCandle }
      in if isEmptyCandle c
           then go (k - 1) ci rolled tr' cs tss (pairs : pss) n
           else go (k - 1) (ci + 1) rolled tr' (c : cs) (reverse ticks : tss) (pairs : pss) (n + 1)

-- run one candle batch; ticks are returned most-recent-first, the head
-- being the end-of-candle marker
runBatch :: SimParams -> Int -> GenState -> LvlTr -> (GenState, [Tick], LvlTr, [(LvlSeg, [LvlEv])])
runBatch sp ci gs0 tr0 =
  let (n, gsR) = rollCount sp gs0
  in finish (go (mkPriceFun sp) n gsR tr0 [] [])
  where
    go _ 0 gs tr acc pacc = (gs, acc, tr, pacc)
    go pf k gs tr acc pacc =
      let (gs', ticks, tr', pairs) = runStep pf sp ci gs tr
      in go pf (k - 1) gs' tr' (ticks : acc) (pairs : pacc)
    finish (gs, acc, tr, pacc) =
      let book = gsBook gs
          marker = Tick ci (Price 0) 0 (snapBids book) (snapAsks book)
      in (gs, marker : concatMap reverse acc, tr, concat (reverse pacc))
    snapBids book = take (spBookDepth sp) (Map.toDescList (bookBids book))
    snapAsks book = take (spBookDepth sp) (Map.toAscList (bookAsks book))

-- one order flow step as tick rows: a row per trade with the book right
-- after that trade; when nothing traded, a single book-only row for the
-- placed order in ModeTickSteps, no row in ModeTickTrades.
-- Also threads the level tracker: the placed volume rests at its price,
-- then every match consumes the same amount from the best bid and the
-- best ask level (pre-match top of book, from the trail)
runStep :: PriceFun -> SimParams -> Int -> GenState -> LvlTr -> (GenState, [Tick], LvlTr, [(LvlSeg, [LvlEv])])
runStep pf sp ci gs tr =
  let (mOrder, gs') = rollOrder pf sp gs
  in case mOrder of
       Nothing -> (gs', [], tr, [])
       Just order ->
         let book1 = placeOrder order (gsBook gs')
             trail = matchTrail book1
             book' = case trail of
               []      -> book1
               matches -> fst (last matches)
             trades = map snd trail
             c' = foldl' (\c t -> updateCandle (tradePrice t) (unVol (tradeVol t)) c) (gsCandle gs') trades
             anchor' = case trades of
               []      -> gsAnchor gs'
               _       -> tradePrice (last trades)
             gs'' = gs' { gsBook = book', gsCandle = c', gsAnchor = anchor' }
             tradeTicks = [ Tick ci (tradePrice t) (unVol (tradeVol t)) (snapBids b) (snapAsks b)
                         | (b, t) <- trail ]
             tr1 = trRest ci (orderSide order) (orderPrice order) (unVol (orderVol order)) tr
             pres = book1 : map fst trail
             (tr2, pairs) = foldl' consume (tr1, []) (zip pres trades)
             consume (t, ss) (pre, ttrade) = case (bestBid pre, bestAsk pre) of
               (Just (bp, _), Just (ap, _)) ->
                 let (t', ps') = trConsume ci bp ap (unVol (tradeVol ttrade)) t
                 in (t', ss ++ ps')
               _ -> (t, ss)
         in case trades of
              [] | spMode sp == ModeTickSteps ->
                     (gs'', [Tick ci (Price 0) 0 (snapBids book1) (snapAsks book1)], tr2, [])
                 | otherwise -> (gs'', [], tr2, [])
              _ -> (gs'', tradeTicks, tr2, pairs)
  where
    snapBids book = take (spBookDepth sp) (Map.toDescList (bookBids book))
    snapAsks book = take (spBookDepth sp) (Map.toAscList (bookAsks book))

-- every match from the placed order onwards, with the book after each match
matchTrail :: Book -> [(Book, Trade)]
matchTrail = go []
  where
    go acc book = case matchStep book of
      Just (book', t) -> go ((book', t) : acc) book'
      Nothing         -> reverse acc

-- --- duplicated from Sim.Gen (see module header) -------------------------

type PriceFun = Book -> Side -> Int -> Price -> Maybe Price

mkPriceFun :: SimParams -> PriceFun
mkPriceFun sp = case (spPriceOffset sp, spClampRest sp) of
  (OffsetTicks, False) -> \book side pd anchor ->
    move side pd <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  (OffsetTicks, True) -> \book side pd anchor ->
    clampOwn book side . move side pd
      <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  (OffsetPercent, False) -> \book side pd anchor ->
    movePct side pd <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  (OffsetPercent, True) -> \book side pd anchor ->
    clampOwn book side . movePct side pd
      <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  where
    r = spTickRange sp

    clampOwn book side (Price p) = case ownBest of
      Nothing -> Price p
      Just t  -> case side of
        Bid -> Price (min p (unPrice t + r))
        Ask -> Price (max p (unPrice t - r))
      where
        ownBest = case side of
          Bid -> fst <$> bestBid book
          Ask -> fst <$> bestAsk book

oppositeRef :: Book -> Side -> Maybe Price
oppositeRef book side = case side of
  Bid -> (fst <$> bestAsk book) <|> (fst <$> bestBid book)
  Ask -> (fst <$> bestBid book) <|> (fst <$> bestAsk book)

midOf' :: Book -> Maybe Price
midOf' book = case (bestBid book, bestAsk book) of
  (Just (bp, _), Just (ap, _)) ->
    Just (Price ((unPrice bp + unPrice ap) `div` 2))
  _ -> Nothing

move :: Side -> Int -> Price -> Price
move Bid = addTicks
move Ask = subtractTicks

-- proportional offset: pd basis points of the reference price; the ask
-- is floored at 1 tick, so percent mode can never quote a non-positive
-- price no matter the tick-range
movePct :: Side -> Int -> Price -> Price
movePct Bid pd (Price p) = Price (p + p * pd `div` 10000)
movePct Ask pd (Price p) = Price (max 1 (p - p * pd `div` 10000))

-- with the zero floor on, an agent refuses to place an order that would
-- rest at a non-positive price (a seller won't pay to sell, a buyer's
-- limit is always positive)
guardFloor :: SimParams -> Maybe Price -> Maybe Price
guardFloor sp mp = case mp of
  Just p | spZeroFloor sp && unPrice p < 1 -> Nothing
  _                                        -> mp

rollOrder :: PriceFun -> SimParams -> GenState -> (Maybe Order, GenState)
rollOrder pf sp gs =
  let (bs, gs1)  = rollProb gs
      (vol, gs2) = rollInt (spVolLo sp, spVolHi sp) gs1
      (pd, gs3)  = rollInt (0, spTickRange sp) gs2
      book       = gsBook gs3
      mOrder :: Maybe (Vol -> Order)
      mOrder
        | bs > spSellThresh sp =
            fmap (Order Ask) (guardFloor sp (pf book Ask pd (gsAnchor gs3)))
        | bs < spBuyThresh sp =
            fmap (Order Bid) (guardFloor sp (pf book Bid pd (gsAnchor gs3)))
        | otherwise = Nothing
  in (mOrder <*> mkVol vol, gs3)

-- order count for one candle, uniform in [lo,hi]; a scalar config
-- (lo==hi) draws no RNG so old runs stay bit-identical
rollCount :: SimParams -> GenState -> (Int, GenState)
rollCount sp gs
  | lo == hi  = (lo, gs)
  | otherwise = rollInt (min lo hi, max lo hi) gs
  where
    lo = spOrdersLo sp
    hi = spOrdersHi sp

rollInt :: (Int, Int) -> GenState -> (Int, GenState)
rollInt (lo, hi) gs =
  let (w, rng') = nextWord64 (gsRng gs)
      range = fromIntegral (hi - lo + 1) :: Word64
  in (lo + fromIntegral (w `mod` range), gs { gsRng = rng' })

rollProb :: GenState -> (Double, GenState)
rollProb gs =
  let (w, rng') = nextWord64 (gsRng gs)
  in (fromIntegral w / fromIntegral (maxBound :: Word64), gs { gsRng = rng' })

addTicks :: Int -> Price -> Price
addTicks d (Price p) = Price (p + d)

subtractTicks :: Int -> Price -> Price
subtractTicks d (Price p) = Price (p - d)
