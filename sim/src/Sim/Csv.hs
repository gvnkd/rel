module Sim.Csv
  ( csvText
  , candleCsv
  , ticksCsv
  , ticksPath
  ) where

import Data.List (intercalate, zipWith4)
import Sim.Types
import Text.Printf (printf)

csvText :: SimParams -> [Candle] -> String
csvText params cs =
  unlines $ "open,high,low,close,vol" : map (candleCsv (spPriceScale params)) cs

candleCsv :: Int -> Candle -> String
candleCsv scale c = printf "%.5f,%.5f,%.5f,%.5f,%d" (f o) (f h) (f l) (f cl) (candleVol c)
  where
    f = priceFloat scale
    o  = candleOpen c
    h  = candleHigh c
    l  = candleLow c
    cl = candleClose c

-- one row per tick: candle idx, trade price/vol (0,0 = book-only row),
-- then book-depth (bid price, bid vol, ask price, ask vol) level quads,
-- padded with 0.0,0 quads past the visible depth
ticksCsv :: SimParams -> [Tick] -> String
ticksCsv params ts =
  unlines $ tickHeader (spBookDepth params) : map (tickRow (spPriceScale params) (spBookDepth params)) ts

tickHeader :: Int -> String
tickHeader d = "c,price,vol," ++ intercalate "," (concatMap col [1 .. d])
  where
    col k = [printf "bp%d,bv%d,ap%d,av%d" k k k k]

tickRow :: Int -> Int -> Tick -> String
tickRow scale d t =
  printf "%d,%.5f,%d" (tickCandle t) (f (tickPrice t)) (tickVol t)
    ++ concat (zipWith4 level bidPs bidVs askPs askVs)
  where
    level bp bv ap av = printf ",%.5f,%d,%.5f,%d" (f (Price bp)) bv (f (Price ap)) av
    bidPs = pad (map (unPrice . fst) (tickBids t))
    bidVs = pad (map (unVol . snd) (tickBids t))
    askPs = pad (map (unPrice . fst) (tickAsks t))
    askVs = pad (map (unVol . snd) (tickAsks t))
    pad xs = take d xs ++ replicate (d - length xs) 0
    f = priceFloat scale

ticksPath :: FilePath -> FilePath
ticksPath path = case break (== '.') (reverse path) of
  (_, [])     -> path ++ ".ticks.csv"
  (_, _ : rb) -> reverse rb ++ ".ticks.csv"

priceFloat :: Int -> Price -> Double
priceFloat scale (Price p) = fromIntegral p / fromIntegral scale
