module Sim.Cli
  ( applyArgs
  , usage
  ) where

import Data.Char (toLower)
import Sim.Types

-- apply "--key value" style arguments over the defaults
applyArgs :: [String] -> Either String SimParams
applyArgs = go defaultParams
  where
    go p [] = Right p
    go p ("--help" : _) = Left usage
    go p ("--seed" : v : rest) = case reads v of
      [(x, "")] -> go p { spSeed = x } rest
      _         -> Left ("invalid --seed value: " ++ v)
    go p ("--vol" : v : rest) =
      let (lo, rest') = span (/= ',') v
      in case (reads lo, reads (drop 1 rest')) of
        ([(l, "")], [(h, "")]) -> go p { spVolLo = l, spVolHi = h } rest
        _                      -> Left ("invalid --vol value (expected lo,hi): " ++ v)
    go p ("--buy-thresh" : v : rest) = case reads v of
      [(x, "")] -> go p { spBuyThresh = x } rest
      _         -> Left ("invalid --buy-thresh value: " ++ v)
    go p ("--sell-thresh" : v : rest) = case reads v of
      [(x, "")] -> go p { spSellThresh = x } rest
      _         -> Left ("invalid --sell-thresh value: " ++ v)
    go p ("--mode" : v : rest) = case parseMode v of
      Just m  -> go p { spMode = m } rest
      Nothing -> Left ("invalid --mode value (expected candles|tick-trades|tick-steps): " ++ v)
    go p ("--clamp-rest" : v : rest) = case parseBool v of
      Just b  -> go p { spClampRest = b } rest
      Nothing -> Left ("invalid --clamp-rest value (expected on|off): " ++ v)
    go p (name : v : rest)
      | Just set <- lookup name setters = case reads v of
          [(x, "")] -> go (set x p) rest
          _         -> Left ("invalid value for " ++ name ++ ": " ++ v)
      | otherwise = Left ("unknown argument: " ++ name)
    go _ [name] = Left ("missing value for " ++ name)

setters :: [(String, Int -> SimParams -> SimParams)]
setters =
  [ ("--candles",           \x s -> s { spCandles = x })
  , ("--orders-per-candle", \x s -> s { spOrdersPerCandle = x })
  , ("--tick-range",        \x s -> s { spTickRange = x })
  , ("--start-bid",         \x s -> s { spStartBid = Price x })
  , ("--start-ask",         \x s -> s { spStartAsk = Price x })
  , ("--start-vol",         \x s -> s { spStartVol = fromMaybeDefault s (mkVol x) })
  , ("--price-scale",       \x s -> s { spPriceScale = x })
  , ("--book-depth",        \x s -> s { spBookDepth = x })
  ]

parseMode :: String -> Maybe SimMode
parseMode "candles"    = Just ModeCandles
parseMode "tick-trades" = Just ModeTickTrades
parseMode "tick-steps" = Just ModeTickSteps
parseMode _            = Nothing

parseBool :: String -> Maybe Bool
parseBool v = case map toLower v of
  "on"    -> Just True
  "off"   -> Just False
  "true"  -> Just True
  "false" -> Just False
  "1"     -> Just True
  "0"     -> Just False
  _       -> Nothing

fromMaybeDefault :: SimParams -> Maybe Vol -> Vol
fromMaybeDefault s = maybe (spStartVol s) id

usage :: String
usage = unlines
  [ "sim — offline candle generator"
  , "usage: sim [options]"
  , "  --candles N            number of candles to generate (default 1000)"
  , "  --orders-per-candle N  orders simulated per candle (default 500)"
  , "  --seed N               RNG seed (default 42)"
  , "  --tick-range N         max distance from best price in ticks (default 10)"
  , "  --vol lo,hi            order volume range (default 1,20)"
  , "  --start-bid P          initial bid price in ticks (default 102190)"
  , "  --start-ask P          initial ask price in ticks (default 102195)"
  , "  --start-vol V          initial book volume (default 200)"
  , "  --price-scale N        ticks per price unit (default 100000)"
  , "  --mode M               candles | tick-trades | tick-steps (default candles)"
  , "  --book-depth N         book levels per side in tick rows (default 50)"
  , "  --clamp-rest B         on | off — rest orders at most tick-range beyond"
  , "                         their side's best (default off)"
  , "output: CSV on stdout (open,high,low,close,vol); in tick modes the UI"
  , "  additionally writes <out>.ticks.csv next to the candles file"
  ]
