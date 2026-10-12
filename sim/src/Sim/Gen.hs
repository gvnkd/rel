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
  }

initState :: SimParams -> GenState
initState sp = GenState
  { gsRng    = mkSMGen (spSeed sp)
  , gsBook   = seedBook
  , gsCandle = emptyCandle
  }
  where
    seedBook =
      placeOrder (Order Ask (spStartAsk sp) (spStartVol sp))
        (placeOrder (Order Bid (spStartBid sp) (spStartVol sp)) emptyBook)

-- one order flow step: a sell hits below the best bid, a buy lifts above the best ask
placeRandomOrder :: SimParams -> GenState -> GenState
placeRandomOrder sp gs =
  let (bs, gs1)  = rollProb gs
      (vol, gs2) = rollInt (spVolLo sp, spVolHi sp) gs1
      (pd, gs3)  = rollInt (0, spTickRange sp) gs2
      book       = gsBook gs3
      mOrder :: Maybe (Vol -> Order)
      mOrder
        | bs > spSellThresh sp =
            (\p v -> Order Ask p v) . subtractTicks pd <$> refPrice book Bid Ask
        | bs < spBuyThresh sp =
            (\p v -> Order Bid p v) . addTicks pd <$> refPrice book Ask Bid
        | otherwise = Nothing
  in case mOrder of
       Just mkOrder -> case mkVol vol of
         Just v  -> gs3 { gsBook = placeOrder (mkOrder v) book }
         Nothing -> gs3
       Nothing -> gs3

-- the reference price for an order on `own` side: the opposite side's best,
-- falling back to our own side's best
refPrice :: Book -> Side -> Side -> Maybe Price
refPrice book own other =
  (fst <$> bestOf book other) <|> (fst <$> bestOf book own)

bestOf :: Book -> Side -> Maybe (Price, Vol)
bestOf book Bid = bestBid book
bestOf book Ask = bestAsk book

runOrders :: SimParams -> Int -> GenState -> GenState
runOrders sp n = go n
  where
    go 0 gs = gs
    go k gs =
      let placed = placeRandomOrder sp gs
          (book', trades) = processOrders (gsBook placed)
          candle' = foldl' (\c t -> updateCandle (tradePrice t) c) (gsCandle placed) trades
      in go (k - 1) placed { gsBook = book', gsCandle = candle' }

-- one candle per spOrdersPerCandle placed orders, seeded book, deterministic
runSim :: SimParams -> [Candle]
runSim sp = go (spCandles sp) (initState sp) []
  where
    go 0 _ acc = reverse acc
    go k gs acc =
      let gs'    = runOrders sp (spOrdersPerCandle sp) gs
          c      = gsCandle gs'
          acc'   = if isEmptyCandle c then acc else c : acc
          rolled = gs' { gsCandle = emptyCandle }
      in acc' `seq` go (k - 1) rolled acc'

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
