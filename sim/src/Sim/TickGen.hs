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
import System.Random.SplitMix (nextWord64)

-- candles (compacted, same as runSim) plus the tick export:
-- in ModeTickTrades one row per trade, in ModeTickSteps additionally one
-- row per placed order that did not trade; every candle ends with a
-- book-only marker row carrying the exact end-of-candle book.
-- per-candle tick lists are collected as chunks and concatenated once
-- at the end: `ts ++ reverse ticks` per candle is quadratic overall.
runSimTicks :: SimParams -> ([Candle], [Tick])
runSimTicks sp = go (spCandles sp) 0 (initState sp) [] []
  where
    go 0 _ _ cs tss = (reverse cs, concat (reverse tss))
    go k ci gs cs tss =
      let (gs', ticks) = runBatch sp ci gs
          c            = gsCandle gs'
          rolled       = gs' { gsCandle = emptyCandle }
      in if isEmptyCandle c
           then go (k - 1) ci rolled cs tss
           else go (k - 1) (ci + 1) rolled (c : cs) (reverse ticks : tss)

-- run one candle batch; ticks are returned most-recent-first, the head
-- being the end-of-candle marker
runBatch :: SimParams -> Int -> GenState -> (GenState, [Tick])
runBatch sp ci gs0 =
  let (n, gsR) = rollCount sp gs0
  in finish (go (mkPriceFun sp) n gsR [])
  where
    go _ 0 gs acc = (gs, acc)
    go pf k gs acc =
      let (gs', ticks) = runStep pf sp ci gs
      in go pf (k - 1) gs' (ticks : acc)
    finish (gs, acc) =
      let book = gsBook gs
          marker = Tick ci (Price 0) 0 (snapBids book) (snapAsks book)
      in (gs, marker : concatMap reverse acc)
    snapBids book = take (spBookDepth sp) (Map.toDescList (bookBids book))
    snapAsks book = take (spBookDepth sp) (Map.toAscList (bookAsks book))

-- one order flow step as tick rows: a row per trade with the book right
-- after that trade; when nothing traded, a single book-only row for the
-- placed order in ModeTickSteps, no row in ModeTickTrades
runStep :: PriceFun -> SimParams -> Int -> GenState -> (GenState, [Tick])
runStep pf sp ci gs =
  let (mOrder, gs') = rollOrder pf sp gs
  in case mOrder of
       Nothing -> (gs', [])
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
         in case trades of
              [] | spMode sp == ModeTickSteps ->
                     (gs'', [Tick ci (Price 0) 0 (snapBids book1) (snapAsks book1)])
                 | otherwise -> (gs'', [])
              _ -> (gs'', tradeTicks)
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
mkPriceFun sp = case spClampRest sp of
  False -> \book side pd anchor ->
    move side pd <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  True -> \book side pd anchor ->
    clampOwn book side . move side pd
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

rollOrder :: PriceFun -> SimParams -> GenState -> (Maybe Order, GenState)
rollOrder pf sp gs =
  let (bs, gs1)  = rollProb gs
      (vol, gs2) = rollInt (spVolLo sp, spVolHi sp) gs1
      (pd, gs3)  = rollInt (0, spTickRange sp) gs2
      book       = gsBook gs3
      mOrder :: Maybe (Vol -> Order)
      mOrder
        | bs > spSellThresh sp =
            Order Ask <$> pf book Ask pd (gsAnchor gs3)
        | bs < spBuyThresh sp =
            Order Bid <$> pf book Bid pd (gsAnchor gs3)
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
