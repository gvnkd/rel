-- continuation state for a generated timeline: enough to resume the exact
-- same run later -- the full SimParams, the RNG state, the live order
-- book, the last-trade anchor, and how many candles are already written
-- (tick-mode sidecar rows are indexed by candle, so a resume must start
-- numbering there). Serialized next to the output: <out-basename>.state.
module Sim.Resume
  ( statePath
  , stateText
  , parseState
  , writeState
  , readState
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Sim.Cli (applyArgs)
import Sim.Gen (GenState (..), initState)
import Sim.Internal
import Text.Read (readMaybe)

statePath :: FilePath -> FilePath
statePath path = case break (== '.') (reverse path) of
  (_, [])     -> path ++ ".state"
  (_, _ : rb) -> reverse rb ++ ".state"

-- a sinus run has no live state: the sequence is closed-form, so the
-- snapshot is just the params and the done count
stateText :: SimParams -> Int -> GenState -> String
stateText sp done gs
  | spSinus sp = unlines
      [ "# rel sim state v1"
      , "params=" ++ paramsString sp
      , "done=" ++ show done
      ]
  | otherwise = unlines $
      [ "# rel sim state v1"
      , "params=" ++ paramsString sp
      , "done=" ++ show done
      , "rng=" ++ show (gsRng gs)
      , "anchor=" ++ show (unPrice (gsAnchor gs))
      , "candle=" ++ candleString (gsCandle gs)
      ]
      ++ map (level "b") (Map.toDescList (bookBids (gsBook gs)))
      ++ map (level "a") (Map.toAscList (bookAsks (gsBook gs)))
  where
    level s (p, v) = s ++ "=" ++ show (unPrice p) ++ "," ++ show (unVol v)
    candleString c = intercalate ","
      [ show (unPrice (candleOpen c))
      , show (unPrice (candleHigh c))
      , show (unPrice (candleLow c))
      , show (unPrice (candleClose c))
      , show (candleVol c)
      ]

paramsString :: SimParams -> String
paramsString sp = intercalate ";"
  [ "seed=" ++ show (spSeed sp)
  , "candles=" ++ show (spCandles sp)
  , "orders-per-candle=" ++ show (spOrdersLo sp) ++ "," ++ show (spOrdersHi sp)
  , "tick-range=" ++ show (spTickRange sp)
  , "vol=" ++ show (spVolLo sp) ++ "," ++ show (spVolHi sp)
  , "start-bid=" ++ show (unPrice (spStartBid sp))
  , "start-ask=" ++ show (unPrice (spStartAsk sp))
  , "start-vol=" ++ show (unVol (spStartVol sp))
  , "buy-thresh=" ++ show (spBuyThresh sp)
  , "sell-thresh=" ++ show (spSellThresh sp)
  , "price-scale=" ++ show (spPriceScale sp)
  , "mode=" ++ modeName (spMode sp)
  , "book-depth=" ++ show (spBookDepth sp)
  , "clamp-rest=" ++ boolString (spClampRest sp)
  , "price-offset=" ++ offsetName (spPriceOffset sp)
  , "zero-floor=" ++ boolString (spZeroFloor sp)
  , "sinus=" ++ boolString (spSinus sp)
  , "sinus-period=" ++ show (spSinusPeriod sp)
  , "sinus-amp=" ++ show (spSinusAmp sp)
  , "sinus-waves=" ++ show (spSinusWaves sp)
  , "sinus-vol-base=" ++ show (spSinusVolBase sp)
  , "sinus-vol-amp=" ++ show (spSinusVolAmp sp)
  ]

boolString :: Bool -> String
boolString b = if b then "on" else "off"

modeName :: SimMode -> String
modeName ModeCandles   = "candles"
modeName ModeTickTrades = "tick-trades"
modeName ModeTickSteps = "tick-steps"

offsetName :: PriceOffset -> String
offsetName OffsetTicks   = "ticks"
offsetName OffsetPercent = "percent"

parseState :: String -> Either String (SimParams, Int, GenState)
parseState content = do
  let ls = [ l | l@(c : _) <- lines content, c /= '#' ]
  sp   <- parseParams ls
  done <- parseKey "done" ls >>= readField "done"
  if spSinus sp
    then pure (sp, done, initState sp)
    else do
      rng  <- parseKey "rng" ls >>= readField "rng"
      anch <- parseKey "anchor" ls >>= readField "anchor"
      cnd  <- parseCandle =<< parseKey "candle" ls
      book <- parseBook ls
      pure (sp, done, GenState rng book cnd (Price anch))
  where
    readField :: Read a => String -> String -> Either String a
    readField k v = maybe (Left ("bad " ++ k ++ ": " ++ v)) Right (readMaybe v)

-- the params line uses the CLI key=value format, so parse it with the
-- CLI argument reader
parseParams :: [String] -> Either String SimParams
parseParams ls = do
  s <- parseKey "params" ls
  applyArgs (concatMap pair (filter (not . null) (split ';' s)))
  where
    pair kv =
      let (k, v) = break (== '=') kv
      in ["--" ++ k, drop 1 v]

    split c s = case break (== c) s of
      (pre, [])       -> [pre]
      (pre, _ : post) -> pre : split c post

parseBook :: [String] -> Either String Book
parseBook ls = do
  bids <- mapM (level "b") [ v | l <- ls, Just v <- [strip "b=" l] ]
  asks <- mapM (level "a") [ v | l <- ls, Just v <- [strip "a=" l] ]
  pure (Book (Map.fromList bids) (Map.fromList asks))
  where
    strip p s = if take (length p) s == p then Just (drop (length p) s) else Nothing
    level side v = case break (== ',') v of
      (pp, ',' : vv) -> do
        p <- readField side pp
        n <- readField side vv
        vol <- maybe (Left ("bad " ++ side ++ " vol: " ++ vv)) Right (mkVol n)
        pure (Price p, vol)
      _ -> Left ("bad book level: " ++ v)
    readField k v = maybe (Left ("bad " ++ k ++ ": " ++ v)) Right (readMaybe v)

parseCandle :: String -> Either String Candle
parseCandle v = case mapM readMaybe (split ',' v) of
  Just [o, h, l, c, vol] -> Right (Candle (Price o) (Price h) (Price l) (Price c) vol)
  _ -> Left ("bad candle: " ++ v)
  where
    split c s = case break (== c) s of
      (pre, [])       -> [pre]
      (pre, _ : post) -> pre : split c post

parseKey :: String -> [String] -> Either String String
parseKey key ls = case [ drop (length key + 1) l | l <- ls, hasKey l ] of
  [v] -> Right v
  []  -> Left ("missing " ++ key ++ " in state")
  _   -> Left ("duplicate " ++ key ++ " in state")
  where
    hasKey l = fst (break (== '=') l) == key

writeState :: FilePath -> SimParams -> Int -> GenState -> IO ()
writeState fp sp done = writeFile fp . stateText sp done

readState :: FilePath -> IO (Either String (SimParams, Int, GenState))
readState fp = parseState <$> readFile fp
