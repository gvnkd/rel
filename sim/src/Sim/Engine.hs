{-# LANGUAGE StrictData #-}

module Sim.Engine
  ( placeOrder
  , bestBid
  , bestAsk
  , processOrders
  , updateCandle
  ) where

import qualified Data.Map.Strict as Map
import Sim.Types

-- vol > 0 lands in bids, vol < 0 in asks (stored absolute); vol == 0 is a no-op
placeOrder :: Price -> Vol -> Book -> Book
placeOrder p v book
  | unVol v == 0 = book
  | unVol v > 0  = book { bookBids = addVol p v (bookBids book) }
  | otherwise    = book { bookAsks = addVol p (Vol (abs (unVol v))) (bookAsks book) }

addVol :: Price -> Vol -> Map.Map Price Vol -> Map.Map Price Vol
addVol p v = Map.insertWith (\new old -> Vol (unVol new + unVol old)) p v

bestBid :: Book -> Maybe (Price, Vol)
bestBid = Map.lookupMax . bookBids

bestAsk :: Book -> Maybe (Price, Vol)
bestAsk = Map.lookupMin . bookAsks

-- repeatedly match top of book; trades are priced at the ask
processOrders :: Book -> (Book, [Tick])
processOrders = go []
  where
    go acc book = case (bestBid book, bestAsk book) of
      (Just (bp, bv), Just (ap, av))
        | bp >= ap ->
            let v = min bv av
                bids' = updateLevel bp bv v (bookBids book)
                asks' = updateLevel ap av v (bookAsks book)
            in go (Tick ap v : acc) book { bookBids = bids', bookAsks = asks' }
      _ -> (book, reverse acc)
    updateLevel p bv v levels
      | bv == v   = Map.delete p levels
      | otherwise = Map.insert p (Vol (unVol bv - unVol v)) levels

-- port of Candle.update: first trade opens, subsequent trades extend h/l and set close
updateCandle :: Price -> Candle -> Candle
updateCandle p c
  | isEmptyCandle c = Candle p p p p
  | p > candleHigh c = c { candleHigh = p, candleClose = p }
  | p < candleLow c  = c { candleLow = p, candleClose = p }
  | otherwise        = c { candleClose = p }
