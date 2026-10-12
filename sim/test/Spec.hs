module Main (main) where

import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sim.Engine
import Sim.Gen
import Sim.Types
import Test.QuickCheck

-- order flows, mirroring the invariants the GDScript suite checks
newtype Flow = Flow [Order]
  deriving (Show)

genSide :: Gen Side
genSide = elements [Bid, Ask]

genPrice :: Gen Price
genPrice = Price <$> chooseInt (0, 100)

genVol :: Gen Vol
genVol = fromJust . mkVol <$> chooseInt (1, 20)

instance Arbitrary Flow where
  arbitrary = Flow <$> listOf1 (Order <$> genSide <*> genPrice <*> genVol)

buildBook :: Flow -> Book
buildBook (Flow orders) = foldl' (flip placeOrder) emptyBook orders

prop_no_crossed_book :: Flow -> Property
prop_no_crossed_book f =
  let book = fst (processOrders (buildBook f))
  in classify (Map.null (bookBids book) || Map.null (bookAsks book)) "one side empty"
     $ case (bestBid book, bestAsk book) of
         (Just (bp, _), Just (ap, _)) -> property (bp < ap)
         _                            -> property True

prop_positive_volumes :: Flow -> Bool
prop_positive_volumes f =
  let book = fst (processOrders (buildBook f))
      vols = Map.elems (bookBids book) ++ Map.elems (bookAsks book)
  in all (\v -> unVol v > 0) vols

prop_trades_at_ask_prices :: Flow -> Bool
prop_trades_at_ask_prices f =
  let book0 = buildBook f
      askPrices = Map.keysSet (bookAsks book0)
      (_, trades) = processOrders book0
  in all (\t -> Set.member (tradePrice t) askPrices) trades

prop_trade_volume_conserved :: Flow -> Bool
prop_trade_volume_conserved (Flow orders) =
  let book0 = foldl' (flip placeOrder) emptyBook orders
      placed = sum (map (unVol . orderVol) orders)
      (_, trades) = processOrders book0
      traded = sum (map (unVol . tradeVol) trades)
  in all (> 0) (map (unVol . tradeVol) trades) && traded <= placed

prop_candle_invariants :: [Int] -> Property
prop_candle_invariants ps0 =
  let ps = map Price ps0
      c = foldl' (flip updateCandle) emptyCandle ps
      mFirst = listToMaybe ps
      mLast = listToMaybe (reverse ps)
  in case (mFirst, mLast) of
       (Just o, Just cl) -> property $
            candleLow c <= min (candleOpen c) (candleClose c)
         && max (candleOpen c) (candleClose c) <= candleHigh c
         && candleOpen c == o
         && candleClose c == cl
       _ -> property True

prop_sim_produces_valid_candles :: Word64 -> Property
prop_sim_produces_valid_candles seed =
  let cs = runSim defaultParams { spSeed = seed, spCandles = 50, spOrdersPerCandle = 100 }
  in not (null cs) ==> all validCandle cs

validCandle :: Candle -> Bool
validCandle c =
  not (isEmptyCandle c)
  && candleLow c <= min (candleOpen c) (candleClose c)
  && max (candleOpen c) (candleClose c) <= candleHigh c

main :: IO ()
main = mapM_ (quickCheckWith (stdArgs { maxSuccess = 500 }))
  [ property prop_no_crossed_book
  , property prop_positive_volumes
  , property prop_trades_at_ask_prices
  , property prop_trade_volume_conserved
  , property prop_candle_invariants
  , property prop_sim_produces_valid_candles
  ]
