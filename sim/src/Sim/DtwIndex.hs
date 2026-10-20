{-# LANGUAGE BangPatterns #-}

-- PAA/SAX-style fingerprint index for subsequence DTW search. A window is
-- summarized by its piecewise aggregate approximation (p segment means of
-- the z-normalized window, O(1) per segment via prefix sums), each mean
-- quantized through equiprobable gaussian breakpoints into a base-`card`
-- digit, digits packed into one Word64. Positions of series B sharing the
-- query window's word (or a single-digit neighbor word, to absorb
-- quantization slack) are the only candidates verified with real banded
-- DTW — random positions reject at the fingerprint stage.
module Sim.DtwIndex
  ( ScanParams (..)
  , defaultScanParams
  , ScanResult (..)
  , breakpoints
  , buildFp
  , mkIndex
  , wordAt
  , wordOfZ
  , neighborWords
  , scanWindows
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU
import Sim.Dtw (Prefix, mkPrefix, pfLen, zslice, stepCost, subseqAt)

data ScanParams = ScanParams
  { scWin     :: !Int     -- window length in A candles
  , scBand    :: !Int     -- Sakoe-Chiba band (X slack), segment = win+2*band
  , scPaa     :: !Int     -- PAA segments per fingerprint
  , scCard    :: !Int     -- quantization cardinality per segment (3..8)
  , scStep    :: !Int     -- A-window slide step
  , scMaxCand :: !Int     -- candidate verifications cap per window
  } deriving (Show)

defaultScanParams :: ScanParams
defaultScanParams = ScanParams 128 32 8 8 32 64

-- one entry per stepped A window: (iA, jB, perStepDist); jB = -1 when no
-- candidate survived verification
data ScanResult = ScanResult
  { srMatches :: !(VU.Vector (Int, Int, Double))
  , srBest    :: !(Maybe (Int, Int, Double))
  } deriving (Show)

-- equiprobable N(0,1) breakpoints for SAX-style quantization
breakpoints :: Int -> VU.Vector Double
breakpoints c = case c of
  3 -> VU.fromList [-0.4307273, 0.4307273]
  4 -> VU.fromList [-0.6744898, 0.0, 0.6744898]
  5 -> VU.fromList [-0.8416212, -0.2533471, 0.2533471, 0.8416212]
  6 -> VU.fromList [-0.9674216, -0.4307273, 0.0, 0.4307273, 0.9674216]
  7 -> VU.fromList [-1.0675705, -0.5659488, -0.1800124, 0.1800124, 0.5659488, 1.0675705]
  _ -> VU.fromList [-1.1503494, -0.6744898, -0.3186394, 0.0, 0.3186394, 0.6744898, 1.1503494]

-- word of a z-normalized window: p PAA segment means quantized to digits.
-- Requires p * (len `div` p) == len (callers pick compatible lengths).
wordOfZ :: VU.Vector Double -> Int -> Int -> Word64
wordOfZ z p card
  | sp < 1 = 0
  | otherwise = foldl' (\w k -> w * fromIntegral card + dig k) 0 [0 .. p - 1]
  where
  n = VU.length z
  sp = n `div` p
  bps = breakpoints card
  dig k =
    let seg = VU.slice (k * sp) sp z
        mu = VU.sum seg / fromIntegral sp
    in fromIntegral (VU.length (VU.filter (< mu) bps))

-- fingerprint word of the z-normalized window at position j of the raw
-- series; positions [0 .. n-win]
wordAt :: Prefix -> Int -> Int -> Int -> Int -> Word64
wordAt pf j win p card = wordOfZ (zslice pf j win) p card

-- word per position over the whole series
buildFp :: Prefix -> Int -> Int -> Int -> VU.Vector Word64
buildFp pf win p card
  | n < win = VU.empty
  | otherwise = VU.generate (n - win + 1) (\j -> wordAt pf j win p card)
  where
  n = pfLen pf

-- inverted index: word -> ascending positions
mkIndex :: VU.Vector Word64 -> Map.Map Word64 [Int]
mkIndex ws = Map.map reverse acc
  where
  acc = VU.ifoldl' (\m j w -> Map.insertWith (\[n] old -> n : old) w [j] m) Map.empty ws

digitAt :: Int -> Int -> Word64 -> Int
digitAt card k w = fromIntegral (w `div` fromIntegral (card ^ k) `mod` fromIntegral card)

replaceDigit :: Int -> Int -> Word64 -> Int -> Word64
replaceDigit card k w d =
  let place = fromIntegral (card ^ k)
      old = fromIntegral (digitAt card k w)
  in w + fromIntegral (d - old) * place

-- words within Hamming distance 2 with per-digit deviation <= 1
-- (quantization slack); excludes the word itself. Cheap because the
-- index lookup stays a Map lookup per word.
neighborWords :: Int -> Int -> Word64 -> [Word64]
neighborWords p card w0 = dedupeW (singles ++ pairs)
  where
  digits = [digitAt card k w0 | k <- [0 .. p - 1]]
  dev k =
    let d = digits !! k
    in [replaceDigit card k w0 d' | d' <- [d - 1, d + 1], d' >= 0, d' < card]
  singles = concatMap dev [0 .. p - 1]
  pairs =
    [ w2
    | k1 <- [0 .. p - 1]
    , k2 <- [k1 + 1 .. p - 1]
    , w1 <- dev k1
    , w2 <- dev k2
    , w2 /= w0
    ]

dedupeW :: [Word64] -> [Word64]
dedupeW = go Set.empty
  where
  go _ [] = []
  go seen (x : rest)
    | Set.member x seen = go seen rest
    | otherwise = x : go (Set.insert x seen) rest

-- slide a window over A (step scStep); for each window verify the index
-- candidates of B with banded subsequence DTW and keep the best per-step
-- distance. Requires win `mod` paa == 0, paa in 1..12, card in 3..8.
scanWindows :: ScanParams -> VU.Vector Double -> VU.Vector Double -> ScanResult
scanWindows sp a b
  | scWin sp `mod` scPaa sp /= 0 = empty
  | scPaa sp < 1 || scPaa sp > 12 = empty
  | scCard sp < 3 || scCard sp > 8 = empty
  | na < win || nb < segLen = empty
  | otherwise = ScanResult matches best
  where
  empty = ScanResult VU.empty Nothing
  win = scWin sp
  band = scBand sp
  segLen = win + 2 * band
  pfa = mkPrefix a
  pfb = mkPrefix b
  na = VU.length a
  nb = VU.length b
  idx = mkIndex (buildFp pfb win (scPaa sp) (scCard sp))
  jMax = nb - segLen
  matches = VU.fromList
    [ (i, jBest, dBest)
    | i <- [0, scStep sp .. na - win]
    , let z = zslice pfa i win
          w = wordOfZ z (scPaa sp) (scCard sp)
          buckets = Map.findWithDefault [] w idx
            : map (\nw -> Map.findWithDefault [] nw idx) (neighborWords (scPaa sp) (scCard sp) w)
          inRange js = [j | j <- js, j <= jMax]
          cands = take (scMaxCand sp) (dedupeOrd (concatMap inRange buckets))
          verified = [ (j, d)
                     | j <- cands
                     , Just r <- [subseqAt z pfb j band]
                     , let d = stepCost r
                     , not (isBad d) ]
          (jBest, dBest) = case verified of
            [] -> (-1, 0 / 0)
            v0 : vs -> foldl' (\acc@(_, da) (j', d') -> if d' < da then (j', d') else acc) v0 vs
    ]
  best = case [m | m@(_, j, _) <- VU.toList matches, j >= 0] of
    [] -> Nothing
    m0 : ms -> Just (foldl' (\acc@(_, _, da) m@(_, _, d') -> if d' < da then m else acc) m0 ms)

-- first-occurrence-wins dedupe (keeps probe-priority order intact)
dedupeOrd :: [Int] -> [Int]
dedupeOrd = go Set.empty
  where
  go _ [] = []
  go seen (x : rest)
    | Set.member x seen = go seen rest
    | otherwise = x : go (Set.insert x seen) rest

isBad :: Double -> Bool
isBad x = isNaN x || isInfinite x
