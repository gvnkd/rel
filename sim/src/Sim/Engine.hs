{-# LANGUAGE StrictData #-}

module Sim.Engine
  ( placeOrder
  , bestBid
  , bestAsk
  , matchStep
  , processOrders
  , updateCandle
  ) where

import qualified Data.Map.Strict as Map
import Sim.Internal

placeOrder :: Order -> Book -> Book
placeOrder (Order side p v) book
  | unVol v <= 0 = book
  | side == Bid  = book { bookBids = addLevel p v (bookBids book) }
  | otherwise    = book { bookAsks = addLevel p v (bookAsks book) }

addLevel :: Price -> Vol -> Map.Map Price Vol -> Map.Map Price Vol
addLevel p v = Map.insertWith (\new old -> Vol (unVol new + unVol old)) p v

bestBid :: Book -> Maybe (Price, Vol)
bestBid = Map.lookupMax . bookBids

bestAsk :: Book -> Maybe (Price, Vol)
bestAsk = Map.lookupMin . bookAsks

-- a single match at the top of the book, priced at the ask; Nothing when
-- the book is uncrossed or a side is empty
matchStep :: Book -> Maybe (Book, Trade)
matchStep book = case (bestBid book, bestAsk book) of
  (Just (bp, bv), Just (ap, av))
    | bp >= ap ->
        let v = min bv av
            bids' = consumeLevel bp bv v (bookBids book)
            asks' = consumeLevel ap av v (bookAsks book)
        in Just (book { bookBids = bids', bookAsks = asks' }, Trade ap v)
  _ -> Nothing
  where
    consumeLevel p bv v levels
      | bv == v   = Map.delete p levels
      | otherwise = Map.insert p (Vol (unVol bv - unVol v)) levels

-- repeatedly match top of book; trades are priced at the ask
processOrders :: Book -> (Book, [Trade])
processOrders = go []
  where
    go acc book = case (bestBid book, bestAsk book) of
      (Just (bp, bv), Just (ap, av))
        | bp >= ap ->
            let v = min bv av
                bids' = consumeLevel bp bv v (bookBids book)
                asks' = consumeLevel ap av v (bookAsks book)
            in go (Trade ap v : acc) book { bookBids = bids', bookAsks = asks' }
      _ -> (book, reverse acc)
    consumeLevel p bv v levels
      | bv == v   = Map.delete p levels
      | otherwise = Map.insert p (Vol (unVol bv - unVol v)) levels

-- first trade opens the candle, later trades extend h/l and set close;
-- volume accumulates traded units
updateCandle :: Price -> Int -> Candle -> Candle
updateCandle p v c
  | isEmptyCandle c  = Candle p p p p v
  | p > candleHigh c = c { candleHigh = p, candleClose = p, candleVol = candleVol c + v }
  | p < candleLow c  = c { candleLow = p, candleClose = p, candleVol = candleVol c + v }
  | otherwise        = c { candleClose = p, candleVol = candleVol c + v }
