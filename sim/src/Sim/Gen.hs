{-# LANGUAGE StrictData #-}

module Sim.Gen
  ( GenState (..)
  , initState
  , runSim
  ) where

import Control.Applicative ((<|>))
import Data.Word (Word64)
import Sim.Engine
import Sim.Internal
import System.Random.SplitMix (SMGen, mkSMGen, nextWord64)

data GenState = GenState
  { gsRng    :: !SMGen
  , gsBook   :: !Book
  , gsCandle :: !Candle
  , gsAnchor :: !Price
  }

initState :: SimParams -> GenState
initState sp = GenState
  { gsRng    = mkSMGen (spSeed sp)
  , gsBook   = seedBook
  , gsCandle = emptyCandle
  , gsAnchor = Price ((unPrice (spStartBid sp) + unPrice (spStartAsk sp)) `div` 2)
  }
  where
    seedBook =
      placeOrder (Order Ask (spStartAsk sp) (spStartVol sp))
        (placeOrder (Order Bid (spStartBid sp) (spStartVol sp)) emptyBook)

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

-- full price for an order on `side` given the book and the tick offset:
-- anchored at the bid/ask mid (falling back to the opposite side's
-- best, then the last trade, when a side is empty); resolved once per
-- run by mkPriceFun so the hot loop sees the clamp constant inlined
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

-- one candle per candle's rolled order count, seeded book, deterministic
runSim :: SimParams -> [Candle]
runSim sp = go (spCandles sp) (initState sp) []
  where
    go 0 _ acc = reverse acc
    go k gs acc =
      let (n, gsR) = rollCount sp gs
          gs'    = runOrders sp n gsR
          c      = gsCandle gs'
          acc'   = if isEmptyCandle c then acc else c : acc
          rolled = gs' { gsCandle = emptyCandle }
      in acc' `seq` go (k - 1) rolled acc'

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
