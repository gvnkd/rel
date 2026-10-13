module Main (main) where

import Data.List (sort)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sim.Engine
import Sim.Gen
import Sim.TickGen (runSimTicks)
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
  let ps = map Price (filter (/= 0) ps0)
      c = foldl' (\c p -> updateCandle p 1 c) emptyCandle ps
      mFirst = listToMaybe ps
      mLast = listToMaybe (reverse ps)
  in case (mFirst, mLast) of
        (Just o, Just cl) -> property $
             candleLow c <= min (candleOpen c) (candleClose c)
          && max (candleOpen c) (candleClose c) <= candleHigh c
          && candleOpen c == o
          && candleClose c == cl
          && candleVol c == length ps
        _ -> property True
prop_sim_produces_valid_candles :: Word64 -> Property
prop_sim_produces_valid_candles seed =
  let cs = runSim defaultParams { spSeed = seed, spCandles = 50, spOrdersLo = 100, spOrdersHi = 100 }
  in not (null cs) ==> all validCandle cs

prop_sim_valid_with_options :: Word64 -> Bool -> Property
prop_sim_valid_with_options seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 100
        , spOrdersHi = 100
        , spClampRest = clamp
        }
      cs = runSim sp
  in not (null cs) ==> all validCandle cs

prop_sim_valid_with_order_range :: Word64 -> Bool -> Property
prop_sim_valid_with_order_range seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 60
        , spOrdersHi = 140
        , spClampRest = clamp
        }
      cs = runSim sp
  in not (null cs) ==> all validCandle cs

-- the book must never stay empty: a run with plenty of orders per candle
-- always produces candles
prop_sim_no_dead_market :: Word64 -> Bool -> Property
prop_sim_no_dead_market seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 20
        , spOrdersLo = 200
        , spOrdersHi = 200
        , spClampRest = clamp
        }
  in property (length (runSim sp) == 20)

prop_ticks_candles_match_runSim :: Word64 -> SimMode -> Bool -> Property
prop_ticks_candles_match_runSim seed mode clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 30
        , spOrdersLo = 50
        , spOrdersHi = 50
        , spMode = mode
        , spClampRest = clamp
        }
      (cs, _ts) = runSimTicks sp
  in property $ cs == runSim sp

prop_ticks_book_snapshots :: Word64 -> SimMode -> Property
prop_ticks_book_snapshots seed mode =
  let sp = defaultParams { spSeed = seed, spCandles = 20, spOrdersLo = 50, spOrdersHi = 50, spMode = mode, spBookDepth = 10 }
      (cs, ts) = runSimTicks sp
      lastVol i = case [tickVol t | t <- ts, tickCandle t == i] of
        [] -> -1
        vs -> last vs
  in counterexample "candle count" (length [ tickCandle t | t <- ts, tickVol t == 0 ] >= length cs)
     .&&. counterexample "marker last" (all (\i -> lastVol i == 0) [0 .. length cs - 1])
     .&&. counterexample "sorted levels" (all sortedLevels ts)

sortedLevels :: Tick -> Bool
sortedLevels t =
  let bidPs = map (unPrice . fst) (tickBids t)
      askPs = map (unPrice . fst) (tickAsks t)
      vols  = map (unVol . snd) (tickBids t ++ tickAsks t)
  in bidPs == reverse (sort bidPs)
     && askPs == sort askPs
     && length bidPs <= 10 && length askPs <= 10
     && all (> 0) vols

instance Arbitrary SimMode where
  arbitrary = elements [ModeCandles, ModeTickTrades, ModeTickSteps]

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
  , property prop_sim_valid_with_options
  , property prop_sim_valid_with_order_range
  , property prop_sim_no_dead_market
  , property prop_ticks_candles_match_runSim
  , property prop_ticks_book_snapshots
  ]
