module Sim.Csv
  ( csvText
  , candleCsv
  ) where

import Sim.Types
import Text.Printf (printf)

csvText :: SimParams -> [Candle] -> String
csvText params cs =
  unlines $ "open,high,low,close" : map (candleCsv (spPriceScale params)) cs

candleCsv :: Int -> Candle -> String
candleCsv scale c = printf "%.5f,%.5f,%.5f,%.5f" (f o) (f h) (f l) (f cl)
  where
    f = priceFloat scale
    o  = candleOpen c
    h  = candleHigh c
    l  = candleLow c
    cl = candleClose c

priceFloat :: Int -> Price -> Double
priceFloat scale (Price p) = fromIntegral p / fromIntegral scale
