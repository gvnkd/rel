{-# LANGUAGE StrictData #-}

-- strategy definition. A strategy is a fixed list of SMA-cross slots: each
-- slot says "when SMA(fast) crosses SMA(slow), switch to this position".
-- The most recent cross among all slots wins; flat before the first cross.
-- Text form is key=value compatible (semicolons separate pairs, commas
-- slots, colons fields): slots=10:50:1,3:20:-1
module Sim.Strat.Rules
  ( Slot (..)
  , Strat (..)
  , validSlot
  , stratText
  , parseStrat
  ) where

import Data.List (intercalate)

data Slot = Slot
  { slFast :: !Int
  , slSlow :: !Int
  , slPos  :: !Int   -- -1 = short, 0 = flat, +1 = long
  } deriving (Eq, Show)

newtype Strat = Strat { stSlots :: [Slot] }
  deriving (Eq, Show)

validSlot :: Slot -> Bool
validSlot s = slFast s >= 2 && slSlow s > slFast s && slPos s >= -1 && slPos s <= 1

stratText :: Strat -> String
stratText st = "slots=" ++ intercalate "," (map slotText (stSlots st))
  where
    slotText s = show (slFast s) ++ ":" ++ show (slSlow s) ++ ":" ++ show (slPos s)

parseStrat :: String -> Either String Strat
parseStrat s =
  case lookup "slots" (kvPairs s) of
    Nothing  -> Left "missing slots="
    Just v -> do
      let parts = filter (not . null) (map trim (split ',' v))
      slots <- mapM parseSlot parts
      if null slots then Left "empty slots" else Right (Strat slots)
  where
    parseSlot f = case mapM readI (split ':' f) of
      Just [fa, sl, po] | validSlot (Slot fa sl po) -> Right (Slot fa sl po)
      _                                             -> Left ("bad slot: " ++ f)
    readI x = case reads x of
      [(n, "")] -> Just n
      _         -> Nothing

kvPairs :: String -> [(String, String)]
kvPairs = map pair . filter (not . null) . map trim . split ';'
  where
    pair kv = let (k, v) = break (== '=') kv in (k, drop 1 v)

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (pre, [])       -> [pre]
  (pre, _ : post) -> pre : split c post

trim :: String -> String
trim = dropWhile isS . reverse . dropWhile isS . reverse
  where
    isS ch = ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
