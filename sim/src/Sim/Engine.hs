{-# LANGUAGE StrictData #-}

module Sim.Engine
  ( placeOrder
  , bestBid
  , bestAsk
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

-- first trade opens the candle, later trades extend h/l and set close
updateCandle :: Price -> Candle -> Candle
updateCandle p c
  | isEmptyCandle c  = Candle p p p p
  | p > candleHigh c = c { candleHigh = p, candleClose = p }
  | p < candleLow c  = c { candleLow = p, candleClose = p }
  | otherwise        = c { candleClose = p }
