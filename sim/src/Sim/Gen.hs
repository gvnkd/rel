{-# LANGUAGE StrictData #-}

module Sim.Gen
  ( GenState (..)
  , initState
  , runSim
  ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Sim.Engine
import Sim.Types
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
      placeOrder (spStartAsk sp) (negateVol (spStartVol sp))
        (placeOrder (spStartBid sp) (spStartVol sp) emptyBook)
    negateVol = Vol . negate . unVol

placeRandomOrder :: SimParams -> GenState -> GenState
placeRandomOrder sp gs =
  let (bs, gs1)    = rollProb gs
      (vol, gs2)   = rollInt (spVolLo sp, spVolHi sp) gs1
      (pd, gs3)    = rollInt (0, spTickRange sp) gs2
      book         = gsBook gs3
      mPrice
        | bs > spSellThresh sp =
            subtractTicks pd <$> (fst <$> bestBid book) `orElse` (subtractTicks pd . fst <$> bestAsk book)
        | bs < spBuyThresh sp =
            addTicks pd <$> (fst <$> bestAsk book) `orElse` (addTicks pd . fst <$> bestBid book)
        | otherwise = Nothing
      signedVol = Vol $ if bs > spSellThresh sp then negate vol else vol
      book' = fromMaybe book (flip (placeOrder' signedVol) book <$> mPrice)
  in gs3 { gsBook = book' }
  where
    placeOrder' v p = placeOrder p v
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

runOrders :: SimParams -> Int -> GenState -> GenState
runOrders sp n = go n
  where
    go 0 gs = gs
    go k gs =
      let placed  = placeRandomOrder sp gs
          (book', ticks) = processOrders (gsBook placed)
          candle' = foldl' (\c t -> updateCandle (tickPrice t) c) (gsCandle placed) ticks
      in go (k - 1) placed { gsBook = book', gsCandle = candle' }

-- one candle per spOrdersPerCandle placed orders, seeded book, deterministic
runSim :: SimParams -> [Candle]
runSim sp = go (spCandles sp) (initState sp) []
  where
    go 0 _ acc = reverse acc
    go k gs acc =
      let gs'   = runOrders sp (spOrdersPerCandle sp) gs
          c     = gsCandle gs'
          acc'  = if isEmptyCandle c then acc else c : acc
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
