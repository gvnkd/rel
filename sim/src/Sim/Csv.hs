module Sim.Csv
  ( csvText
  , csvRowsText
  , candleCsv
  , ticksCsv
  , tickHeader
  , tickRow
  , tickRowB
  , ticksPath
  , lvlHeader
  , lvlRowB
  , levelsPath
  , vevHeader
  , vevRecB
  , vevPath
  , tradesBin
  , parseTradesBin
  , metaText
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString.Builder (Builder, byteString, char8, doubleLE, intDec, string8, word32LE, word64LE)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import Data.List (foldl', intercalate, zipWith4)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import GHC.Float (castWord64ToDouble)
import Sim.Lvl (LvlEv (..), LvlSeg (..))
import qualified Sim.Strat.Backtest as STB
import Sim.Types
import Text.Printf (printf)

csvText :: SimParams -> [Candle] -> String
csvText params cs =
  unlines $ "open,high,low,close,vol" : map (candleCsv (spPriceScale params)) cs

-- rows only, no header: appended to an existing candles file when a run
-- is resumed
csvRowsText :: SimParams -> [Candle] -> String
csvRowsText params =
  unlines . map (candleCsv (spPriceScale params))

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

-- Builder variant of tickRow for the streaming FFI path: prices are
-- p / 10^decimals exactly, so the integer is formatted directly -- the
-- output is identical to printf "%.5f" of the double (the decimal
-- expansion has exactly 5 digits, well within double precision) and
-- avoids float formatting entirely
tickRowB :: Int -> Int -> Tick -> Builder
tickRowB scale d t =
  intDec (tickCandle t) <> char8 ',' <> priceB scale (tickPrice t) <> char8 ',' <> intDec (tickVol t)
    <> mconcat (zipWith4 level bidPs bidVs askPs askVs)
  where
    level bp bv ap av =
      char8 ',' <> priceB scale (Price bp) <> char8 ',' <> intDec bv
        <> char8 ',' <> priceB scale (Price ap) <> char8 ',' <> intDec av
    bidPs = pad (map (unPrice . fst) (tickBids t))
    bidVs = pad (map (unVol . snd) (tickBids t))
    askPs = pad (map (unPrice . fst) (tickAsks t))
    askVs = pad (map (unVol . snd) (tickAsks t))
    pad xs = take d xs ++ replicate (d - length xs) 0

-- integer price formatting: prices are p / 10^decimals exactly, so the
-- integer is formatted directly — identical to printf "%.5f" of the
-- double and avoids float formatting entirely
priceB :: Int -> Price -> Builder
priceB scale (Price p) =
  (if p < 0 then char8 '-' else mempty)
    <> intDec w <> char8 '.' <> padDec decimals frac
  where
    w = abs p `div` scale
    frac = abs p `mod` scale
    decimals = decimalsOf scale

    -- zero-pad a fraction to `digits` columns
    padDec digits v = go (digits - digitCount v) <> intDec v
      where
        go n | n <= 0    = mempty
             | otherwise = char8 '0' <> go (n - 1)

    digitCount v = loop 0 (max 1 v)
      where
        loop n x | x < 10    = n + 1
                 | otherwise = loop (n + 1) (x `div` 10)

-- one row per continuous level lifetime (Sim.Lvl): first/last candle of
-- the existence, side (0 = bid, 1 = ask), price, peak resting volume,
-- and the volume-change event range (record indices into the .vev
-- sidecar; evn 0 on rows written before the event export existed)
lvlHeader :: String
lvlHeader = "c0,c1,side,price,peak,ev0,evn"

lvlRowB :: Int -> LvlSeg -> Builder
lvlRowB scale (LvlSeg c0 c1 side px peak ev0 evn) =
  intDec c0 <> char8 ',' <> intDec c1 <> char8 ','
    <> intDec (sideCode side) <> char8 ','
    <> priceB scale px <> char8 ',' <> intDec peak
    <> char8 ',' <> intDec ev0 <> char8 ',' <> intDec evn
  where
    sideCode Bid = 0
    sideCode Ask = 1

-- volume-change event sidecar for OBVOL dynamics: fixed 20-byte
-- records — candle u32 LE, side u32 LE (0 = bid, 1 = ask), integer
-- price u32 LE, absolute level volume u64 LE. Header: magic "RELV" +
-- version u32 + record count u64 (patched when the file is complete).
-- Each lifetime's events are contiguous (see Sim.Lvl), so the UI reads
-- them by seeking to ev0 * 20 + 16
vevHeader :: Builder
vevHeader = string8 "RELV" <> word32LE 1 <> word64LE 0

vevRecB :: LvlEv -> Builder
vevRecB (LvlEv c side (Price px) vol) =
  word32LE (fromIntegral c)
    <> word32LE (sideCode side)
    <> word32LE (fromIntegral px)
    <> word64LE (fromIntegral vol)
  where
    sideCode Bid = 0
    sideCode Ask = 1

vevPath :: FilePath -> FilePath
vevPath path = sidecarPath "vev" path

-- --- backtest trades binary (RELT) -------------------------------------------
--
-- little-endian, records after a 16-byte header:
--   u32 magic 'RELT', u32 version (1), u64 trade count,
--   then count x 48-byte records:
--     i0 u32, i1 u32, side i32, causeId u32,
--     entry f64, exit f64, pnl f64, vol f64
--   then the string table: u64 byte length followed by the cause strings
--   concatenated with NUL separators, in causeId order (UTF-8)

magicTrades :: Word32
magicTrades = 0x544C4552

tradesBin :: [STB.Trade] -> Builder
tradesBin ts =
  word32LE magicTrades
    <> word32LE 1
    <> word64LE (fromIntegral (length ts))
    <> mconcat (map recB ts)
    <> word64LE (fromIntegral (B.length blob))
    <> byteString blob
  where
    causeIds = Map.fromList (zip causes [0 ..])
    causes = dedup (map STB.trCause ts)
    dedup = foldr (\c acc -> if c `elem` acc then acc else c : acc) []
    recB t =
      word32LE (fromIntegral (STB.trI0 t))
        <> word32LE (fromIntegral (STB.trI1 t))
        <> word32LE (fromIntegral (STB.trSide t))
        <> word32LE (causeIds Map.! STB.trCause t)
        <> doubleLE (STB.trEntry t)
        <> doubleLE (STB.trExit t)
        <> doubleLE (STB.trPnl t)
        <> doubleLE (STB.trVol t)
    blob = B.concat (map (B8.pack . (++ "\NUL")) causes)

parseTradesBin :: B.ByteString -> Either String [STB.Trade]
parseTradesBin bs
  | B.length bs < 16 = Left "trades binary too short"
  | getU32 bs 0 /= magicTrades = Left "bad magic"
  | getU32 bs 4 /= 1 = Left "unsupported trades version"
  | B.length bs < 16 + n * recLen = Left "truncated trade records"
  | B.length bs < 16 + n * recLen + 8 = Left "missing string table"
  | B.length bs < 16 + n * recLen + 8 + blobLen = Left "truncated string table"
  | otherwise = mapM recAt [0 .. n - 1]
  where
    n = fromIntegral (getU64 bs 8)
    recLen = 48
    blobOff = 16 + n * recLen
    blobLen = fromIntegral (getU64 bs blobOff)
    blob = B.take blobLen (B.drop (blobOff + 8) bs)
    causes = B.split 0 blob
    causeAt k = if k < length causes
                  then Right (B8.unpack (causes !! k))
                  else Left ("cause id out of range: " ++ show k)
    recAt k =
      let o = 16 + k * recLen
          side = fromIntegral (getI32 bs (o + 8))
          entry = getF64 bs (o + 16)
          exit = getF64 bs (o + 24)
          pnl = getF64 bs (o + 32)
          vol = getF64 bs (o + 40)
      in do
           cause <- causeAt (fromIntegral (getU32 bs (o + 12)))
           Right (STB.Trade { STB.trI0 = fromIntegral (getU32 bs o)
                        , STB.trI1 = fromIntegral (getU32 bs (o + 4))
                        , STB.trSide = side
                        , STB.trCause = cause
                        , STB.trEntry = entry
                        , STB.trExit = exit
                        , STB.trPnl = pnl
                        , STB.trVol = vol })

getU32 :: B.ByteString -> Int -> Word32
getU32 bs o = foldl' (.|.) 0 [fromIntegral (B.index bs (o + i)) `shiftL` (8 * i) | i <- [0 .. 3]]

getU64 :: B.ByteString -> Int -> Word64
getU64 bs o = foldl' (.|.) 0 [fromIntegral (B.index bs (o + i)) `shiftL` (8 * i) | i <- [0 .. 7]]

getI32 :: B.ByteString -> Int -> Int
getI32 bs o = fromIntegral (fromIntegral (getU32 bs o) :: Int32)

getF64 :: B.ByteString -> Int -> Double
getF64 bs o = castWord64ToDouble (getU64 bs o)

ticksPath :: FilePath -> FilePath
ticksPath path = sidecarPath "ticks.csv" path

levelsPath :: FilePath -> FilePath
levelsPath path = sidecarPath "levels.csv" path

sidecarPath :: String -> FilePath -> FilePath
sidecarPath ext path = case break (== '.') (reverse path) of
  (_, [])     -> path ++ "." ++ ext
  (_, _ : rb) -> reverse rb ++ "." ++ ext

priceFloat :: Int -> Price -> Double
priceFloat scale (Price p) = fromIntegral p / fromIntegral scale

-- sidecar metadata for the UI: how many decimals generated prices carry
metaText :: SimParams -> String
metaText params = printf "decimals=%d\n" (decimalsOf (spPriceScale params))

decimalsOf :: Int -> Int
decimalsOf = go 0
  where
    go n s
      | s > 1 && s `mod` 10 == 0 = go (n + 1) (s `div` 10)
      | otherwise                = n
