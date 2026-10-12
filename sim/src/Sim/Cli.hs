module Sim.Cli
  ( applyArgs
  , usage
  ) where

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
  ]

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
  , "output: CSV on stdout (open,high,low,close)"
  ]
