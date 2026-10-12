{-# LANGUAGE StrictData #-}

-- hooked driver for the FFI/UI path: same order flow as Sim.Gen, plus
-- per-candle progress reporting and a cancellation poll.
--
-- the rolling helpers below intentionally duplicate Sim.Gen instead of
-- sharing them: exporting the small worker functions from Sim.Gen costs
-- ~40% of runSim throughput (measured 11s -> 16s on 100M orders), and
-- adding the IO driver to Sim.Gen itself starves the simplifier budget
-- and degrades runSim the same way (measured 11.7s -> 15-16s).
module Sim.GenH
  ( runSimH
  ) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Word (Word64)
import Sim.Engine
import Sim.Gen (GenState (..), initState)
import Sim.Internal
import System.Random.SplitMix (SMGen, nextWord64)

-- like Sim.Gen.runSim but driven in IO: reports per-candle progress and
-- polls for cancellation once per candle (throws SimCancelled); the
-- per-order hot loop below stays pure
runSimH :: SimHooks -> SimParams -> IO [Candle]
runSimH hooks sp = go (spCandles sp) (initState sp) []
  where
    total = spCandles sp
    go 0 _ acc = pure (reverse acc)
    go k gs acc = do
      cancelled <- hookCancelled hooks
      when cancelled (throwIO SimCancelled)
      let gs'    = runOrders sp (spOrdersPerCandle sp) gs
          c      = gsCandle gs'
          acc'   = if isEmptyCandle c then acc else c : acc
          rolled = gs' { gsCandle = emptyCandle }
      hookProgress hooks (total - k + 1) total
      acc' `seq` go (k - 1) rolled acc'

-- --- duplicated from Sim.Gen (see module header) -------------------------

-- one order flow step: a sell hits below the best bid, a buy lifts above the best ask
placeRandomOrder :: PriceFun -> SimParams -> GenState -> GenState
placeRandomOrder pf sp gs =
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
  in case mOrder of
       Just mkOrder -> case mkVol vol of
         Just v  -> gs3 { gsBook = placeOrder (mkOrder v) book }
         Nothing -> gs3
       Nothing -> gs3

-- full price for an order on `side` given the book and the tick offset;
-- resolved once per run by mkPriceFun so the hot loop sees the mode
-- constants inlined
type PriceFun = Book -> Side -> Int -> Price -> Maybe Price

mkPriceFun :: SimParams -> PriceFun
mkPriceFun sp = case (spRefMode sp, spClampRest sp) of
  (RefOppositeBest, False) -> \book side pd _ ->
    move side pd <$> oppositeRef book side
  (RefOppositeBest, True) -> \book side pd _ ->
    clampOwn book side . move side pd <$> oppositeRef book side
  (RefMid, False) -> \book side pd anchor ->
    move side pd <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  (RefMid, True) -> \book side pd anchor ->
    clampOwn book side . move side pd
      <$> (midOf' book <|> oppositeRef book side <|> Just anchor)
  where
    r = spTickRange sp

    -- with clamping on, the price may not rest further than tick-range
    -- beyond our side's best
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

runOrders :: SimParams -> Int -> GenState -> GenState
runOrders sp n gs = go (mkPriceFun sp) n gs
  where
    go _ 0 gs' = gs'
    go pf k gs' =
      let placed = placeRandomOrder pf sp gs'
          (book', trades) = processOrders (gsBook placed)
          candle' = foldl' (\c t -> updateCandle (tradePrice t) (unVol (tradeVol t)) c) (gsCandle placed) trades
          anchor' = case trades of
            []      -> gsAnchor placed
            _       -> tradePrice (last trades)
      in go pf (k - 1) placed { gsBook = book', gsCandle = candle', gsAnchor = anchor' }

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
