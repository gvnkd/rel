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
-- book-only marker row carrying the exact end-of-candle book
runSimTicks :: SimParams -> ([Candle], [Tick])
runSimTicks sp = go (spCandles sp) 0 (initState sp) [] []
  where
    go 0 _ _ cs ts = (reverse cs, ts)
    go k ci gs cs ts =
      let (gs', ticks) = runBatch sp ci gs
          c            = gsCandle gs'
          rolled       = gs' { gsCandle = emptyCandle }
      in if isEmptyCandle c
           then go (k - 1) ci rolled cs ts
           else go (k - 1) (ci + 1) rolled (c : cs) (ts ++ reverse ticks)

-- run one candle batch; ticks are returned most-recent-first, the head
-- being the end-of-candle marker
runBatch :: SimParams -> Int -> GenState -> (GenState, [Tick])
runBatch sp ci gs0 = finish (go (spOrdersPerCandle sp) gs0 [])
  where
    go 0 gs acc = (gs, acc)
    go k gs acc =
      let (gs', ticks) = runStep sp ci gs
      in go (k - 1) gs' (reverse ticks ++ acc)
    finish (gs, acc) =
      let book = gsBook gs
          marker = Tick ci (Price 0) 0 (snapBids book) (snapAsks book)
      in (gs, marker : acc)
    snapBids book = take (spBookDepth sp) (Map.toDescList (bookBids book))
    snapAsks book = take (spBookDepth sp) (Map.toAscList (bookAsks book))

-- one order flow step as tick rows: a row per trade with the book right
-- after that trade; when nothing traded, a single book-only row for the
-- placed order in ModeTickSteps, no row in ModeTickTrades
runStep :: SimParams -> Int -> GenState -> (GenState, [Tick])
runStep sp ci gs =
  let (mOrder, gs') = rollOrder sp gs
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
             gs'' = gs' { gsBook = book', gsCandle = c' }
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

rollOrder :: SimParams -> GenState -> (Maybe Order, GenState)
rollOrder sp gs =
  let (bs, gs1)  = rollProb gs
      (vol, gs2) = rollInt (spVolLo sp, spVolHi sp) gs1
      (pd, gs3)  = rollInt (0, spTickRange sp) gs2
      book       = gsBook gs3
      mOrder :: Maybe (Vol -> Order)
      mOrder
        | bs > spSellThresh sp =
            Order Ask . subtractTicks pd <$> refPrice book Ask Bid
        | bs < spBuyThresh sp =
            Order Bid . addTicks pd <$> refPrice book Bid Ask
        | otherwise = Nothing
  in (mOrder <*> mkVol vol, gs3)

refPrice :: Book -> Side -> Side -> Maybe Price
refPrice book own other =
  (fst <$> bestOf book other) <|> (fst <$> bestOf book own)

bestOf :: Book -> Side -> Maybe (Price, Vol)
bestOf book Bid = bestBid book
bestOf book Ask = bestAsk book

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
