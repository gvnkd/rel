{-# LANGUAGE StrictData #-}

-- tabular feature windows for the offline trainer. A Bar stream (from a
-- sim candles csv or a real-quotes csv) is turned into per-candle feature
-- rows + next-move labels; rows before the warm-up are empty and carry
-- label -1, so the trainer simply enumerates [warmup, n-2].
module Sim.ML.Data
  ( Bar (..)
  , FeatureSpec (..)
  , defaultFeatureSpec
  , featureSize
  , parseBars
  , loadBars
  , parseBarsBinary
  , FeatureSet (..)
  , buildFeatures
  ) where

import Data.Bits (shiftL, (.|.))
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Word (Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import GHC.Float (castWord64ToDouble)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.Entropy (rollingPermEntropy)

-- one candle as doubles; ML pipeline works in returns/ratios, so no
-- integer Price/scale is needed here
data Bar = Bar
  { barO :: !Double
  , barH :: !Double
  , barL :: !Double
  , barC :: !Double
  , barV :: !Double
  } deriving (Eq, Show)

data FeatureSpec = FeatureSpec
  { fsLags      :: [Int]    -- close/close lag-N - 1 returns
  , fsVolWin    :: !Int     -- realized vol (std of lag-1 returns) window
  , fsRangeWin  :: !Int     -- close position in the [low,high] envelope
  , fsSmaWin    :: !Int     -- distance from the SMA(window)
  , fsVolSmaWin :: !Int     -- volume vs its SMA - 1
  , fsKsWin     :: !Int     -- rolling permutation entropy window (0 = off)
  , fsKsM       :: !Int     -- ordinal pattern length for the entropy
  , fsLabelEps  :: !Double  -- |next return| below this = flat class
  } deriving (Eq, Show)

defaultFeatureSpec :: FeatureSpec
defaultFeatureSpec = FeatureSpec
  { fsLags      = [1, 5, 20]
  , fsVolWin    = 20
  , fsRangeWin  = 20
  , fsSmaWin    = 20
  , fsVolSmaWin = 20
  , fsKsWin     = 0
  , fsKsM       = 5
  , fsLabelEps  = 1e-5
  }

featureSize :: FeatureSpec -> Int
featureSize fs =
  length (fsLags fs) + 4 + (if fsKsWin fs > 0 then 1 else 0)

-- parse a candles csv: the sim format (open,high,low,close,vol) or the
-- real-quotes format with a leading timestamp column (YYYY.MM.DD HH:MM,...)
-- — the timestamp column is detected per file by trying the 5-field row
-- first. Unparseable rows are skipped; a file with no usable rows is an
-- error.
parseBars :: String -> Either String [Bar]
parseBars content =
  let lns = filter (not . null) (map trim (lines content))
      rows = concatMap parseLine lns
  in if null rows
       then Left "no parseable candle rows"
       else Right rows
  where
    parseLine ln =
      let f = splitCommas ln
          try5 = barFrom (take 5 f)
      in case try5 of
           Just b  -> [b]
           Nothing -> maybe [] (: []) (barFrom (take 5 (drop 1 f)))
    barFrom fs =
      case mapM readD fs of
        Just [o, h, l, c, v] | o > 0 && h > 0 && l > 0 && c > 0 -> Just (Bar o h l c v)
        _ -> Nothing

data FeatureSet = FeatureSet
  { fRows   :: !(V.Vector (VU.Vector Double)) -- per candle; empty before warm-up
  , fLabels :: !(VU.Vector Int)               -- class of step i -> i+1; -1 = none
  , fWarmup :: !Int
  , fSize   :: !Int
  , fCount  :: !Int
  } deriving (Show)

-- classes: 0 = down, 1 = flat, 2 = up
buildFeatures :: FeatureSpec -> [Bar] -> FeatureSet
buildFeatures spec bars
  | null bars = FeatureSet V.empty VU.empty 0 (featureSize spec) 0
  | otherwise =
      let n = length bars
          o = VU.fromList (map barO bars)
          h = VU.fromList (map barH bars)
          l = VU.fromList (map barL bars)
          c = VU.fromList (map barC bars)
          v = VU.fromList (map barV bars)
          ret = VU.generate n $ \i ->
            if i == 0 then 0 else c VU.! i / c VU.! (i - 1) - 1
          sr  = cumsum ret
          sr2 = cumsum (VU.map (\r -> r * r) ret)
          sc  = cumsum c
          sv  = cumsum v
          ks = if fsKsWin spec > 0
                 then VU.fromList (rollingPermEntropy (fsKsWin spec) (fsKsM spec) (VU.toList c))
                 else VU.empty
          warmup = maximum (1 : fsLags spec ++ [ fsVolWin spec, fsRangeWin spec
                                               , fsSmaWin spec, fsVolSmaWin spec
                                               , fsKsWin spec ])
          size = featureSize spec
          row i = VU.generate size $ \k ->
            let nl = length (fsLags spec)
            in if k < nl
                 then c VU.! i / c VU.! (i - (fsLags spec !! k)) - 1
                 else let k' = k - nl
                      in featureAt k' i
          featureAt 0 i = stdOf sr sr2 (fsVolWin spec) i
          featureAt 1 i =
            let w = fsRangeWin spec
                lo = minimum (VU.toList (VU.slice (i - w + 1) w l))
                hi = maximum (VU.toList (VU.slice (i - w + 1) w h))
            in (c VU.! i - lo) / (hi - lo + 1e-12)
          featureAt 2 i = c VU.! i / meanOf sc (fsSmaWin spec) i - 1
          featureAt 3 i =
            let mv = meanOf sv (fsVolSmaWin spec) i
            in if mv > 0 then v VU.! i / mv - 1 else 0
          featureAt _ i
            | fsKsWin spec > 0 && i < VU.length ks =
                let x = ks VU.! i in if isNaN x then 0.5 else x
            | otherwise = 0.5
          rows = V.generate n $ \i ->
            if i >= warmup && i < n then row i else VU.empty
          labels = VU.generate n $ \i ->
            if i >= warmup && i < n - 1
              then let r = c VU.! (i + 1) / c VU.! i - 1
                   in if r > fsLabelEps spec then 2
                      else if r < negate (fsLabelEps spec) then 0
                      else 1
              else -1
      in rows `seq` FeatureSet rows labels warmup size n

-- inclusive cumsum
cumsum :: VU.Vector Double -> VU.Vector Double
cumsum xs = VU.postscanl' (+) 0 xs

meanOf :: VU.Vector Double -> Int -> Int -> Double
meanOf sc w i = (sc VU.! i - sc VU.! (i - w)) / fromIntegral w

stdOf :: VU.Vector Double -> VU.Vector Double -> Int -> Int -> Double
stdOf sr sr2 w i =
  let m = (sr VU.! i - sr VU.! (i - w)) / fromIntegral w
      m2 = (sr2 VU.! i - sr2 VU.! (i - w)) / fromIntegral w
  in sqrt (max 0 (m2 - m * m))

-- --- binary candles (.rbin) -----------------------------------------------
--
-- both store formats share the record layout (48B LE): time u64,
-- open/high/low/close f64, vol u64. RELQ ("RELQ") = real-quotes store:
-- magic u32@0, version u32@4, has_time u32@8, tf_minutes u32@12,
-- count u64@16, min_low f64@24, max_high f64@32, decimals s32@40.
-- RELS ("RELS") = sim full export: magic u32@0, version u32@4,
-- flags u32@8, decimals s32@12, candle_count u64@16, then book_depth /
-- level_rows / vev_count u64s and min_low/max_high f64 — we take the
-- candles and stop reading before the books/levels/vev blocks.

magicRelQ, magicRelS :: Word32
magicRelQ = 0x514C4552
magicRelS = 0x534C4552

-- sniff the magic and parse accordingly: binary for .rbin stores, csv
-- (sim or real-quotes text) otherwise
loadBars :: FilePath -> IO (Either String [Bar])
loadBars path = do
  bs <- B.readFile path
  if B.length bs >= 68 && (getU32 bs 0 == magicRelQ || getU32 bs 0 == magicRelS)
    then pure (parseBarsBinary bs)
    else pure (parseBars (BC8.unpack bs))

parseBarsBinary :: B.ByteString -> Either String [Bar]
parseBarsBinary bs
  | B.length bs < 64              = Left "binary too short"
  | getU32 bs 0 /= magicRelQ && getU32 bs 0 /= magicRelS = Left "bad magic"
  | getU32 bs 4 /= 1              = Left "unsupported binary version"
  | B.length bs < 64 + n * recLen = Left "truncated candle records"
  | otherwise = Right (map recAt [0 .. n - 1])
  where
    n = fromIntegral (getU64 bs 16)
    recLen = 48
    recAt k =
      let o = 64 + k * recLen
      in Bar (getF64 bs (o + 8)) (getF64 bs (o + 16)) (getF64 bs (o + 24))
             (getF64 bs (o + 32)) (fromIntegral (getU64 bs (o + 40)))

getU32 :: B.ByteString -> Int -> Word32
getU32 bs o = foldl' (.|.) 0 [fromIntegral (B.index bs (o + i)) `shiftL` (8 * i) | i <- [0 .. 3]]

getU64 :: B.ByteString -> Int -> Word64
getU64 bs o = foldl' (.|.) 0 [fromIntegral (B.index bs (o + i)) `shiftL` (8 * i) | i <- [0 .. 7]]

getF64 :: B.ByteString -> Int -> Double
getF64 bs o = castWord64ToDouble (getU64 bs o)

splitCommas :: String -> [String]
splitCommas s = case break (== ',') s of
  (pre, [])       -> [pre]
  (pre, _ : post) -> pre : splitCommas post

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

readD :: String -> Maybe Double
readD s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
