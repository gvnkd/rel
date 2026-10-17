{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Bull-Sideways-Bear probabilities (BSBP) + a 3x3 Markov transition matrix
-- with surprise-adaptive memory.
--
-- Every bar is classified from the SUM of the close-to-close returns over
-- the last `mpWin` bars ENDING at it (partial sum while the window is
-- still filling, so every bar from bar 1 on is classifiable: bar 3 sums
-- returns [1..3]). The state is Bull when the sum >= mpThresh percent,
-- Bear when <= -mpThresh (exactly +-mpThresh is Sideways), otherwise
-- Sideways. This makes a run of moderate moves read as one Bull/Bear
-- state, which no per-bar spike classifier can express.
--
-- The BSBP block is the plain rolling state distribution over the last
-- `mpWin` bars. The Markov block keeps decayed transition counts C: at
-- each observed transition C = lambda * C + 1 on the realized cell,
-- rows normalized (a never-visited state falls back to a uniform row).
-- The estimate's log-loss -log T[prev][cur] is the per-bar surprise; its
-- rolling mean S drives lambda: S above the reference mpSRef shrinks
-- memory towards mpLamLo (the model was wrong, distrust old counts),
-- below it stretches memory towards mpLamHi (trust the regime).
--
-- markovSeries emits a flat 15-double record per bar:
--   +0..2  BSBP bull/sw/bear (NaN until the window fills)
--   +3..11 transition matrix, row-major (Bull, SW, Bear rows)
--   +12    surprise (log-loss of the PREVIOUS bar's matrix vs this bar)
--   +13    rolling surprise S
--   +14    lambda used for this bar's count update
-- The transition matrix and the surprise chain are path-dependent: they
-- start from a uniform prior at the beginning of the GIVEN series (the
-- FFI caller passes a slice; classification is expected to be exact for
-- the slice — see statesOf — only the count/surprise chain restarts).
module Sim.Markov (MClass (..), classify, MarkovParams (..), defaultMarkovParams, markovSeries, markovSeriesStates, statesOf, sanitizeParams) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.List (scanl')
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data MClass = MBull | MSw | MBear deriving (Eq, Show, Enum, Bounded)

-- state of a bar from its window-sum of close-to-close returns in
-- percent
classify :: Double -> Double -> MClass
classify th r
  | r >= th = MBull
  | r < negate th = MBear
  | otherwise = MSw

data MarkovParams = MarkovParams
  { mpWin     :: !Int
  -- ^ rolling window (bars) for BOTH the BSBP histogram and the
  -- classification window-sum
  , mpSurpWin :: !Int
  -- ^ rolling surprise window (bars)
  , mpThresh  :: !Double
  -- ^ bull/bear window-sum threshold, percent
  , mpLamHi   :: !Double
  -- ^ decay factor while surprise stays below the reference (long memory)
  , mpLamLo   :: !Double
  -- ^ decay factor while surprise is high (short memory)
  , mpSRef    :: !Double
  -- ^ surprise reference: log-loss below this keeps lambda at mpLamHi
  , mpK       :: !Double
  -- ^ how fast lambda shrinks as surprise exceeds the reference
  }
  deriving (Show)

defaultMarkovParams :: MarkovParams
defaultMarkovParams = MarkovParams 20 20 5.0 0.99 0.90 0.55 4.0

-- clamps FFI/UI input into a workable range: lambda in (0, 1) with
-- lamLo <= lamHi, non-negative k, positive threshold
sanitizeParams :: MarkovParams -> MarkovParams
sanitizeParams p = p
  { mpWin = max 1 (mpWin p)
  , mpSurpWin = max 1 (mpSurpWin p)
  , mpThresh = if mpThresh p > 0 then mpThresh p else mpThresh defaultMarkovParams
  , mpLamHi = clamp 0.50 0.9999 (mpLamHi p)
  , mpLamLo = clamp 0.50 0.9999 (min (mpLamLo p) (mpLamHi p))
  , mpSRef = if mpSRef p > 0 then mpSRef p else mpSRef defaultMarkovParams
  , mpK = max 0 (mpK p)
  }

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

recordSize :: Int
recordSize = 15

nan :: Double
nan = 0 / 0

-- per-bar states (0 Bull, 1 SW, 2 Bear) of bars 1..n-1 from the window
-- sums; index 0 is a dummy (bar 0 has no return). Used by the FFI path
-- where the caller classifies the whole loaded slice exactly and only
-- the count/surprise chain runs on the subslice
statesOf :: MarkovParams -> [Double] -> VU.Vector Int
statesOf p0 xs = VU.fromList (1 : [st i | i <- [1 .. n - 1]])
  where
  p = sanitizeParams p0
  n = length xs
  v = VU.fromList xs
  win = mpWin p
  th = mpThresh p
  ret i = ((v VU.! i) / (v VU.! (i - 1)) - 1) * 100
  pres = VU.fromList (scanl' (+) 0 [ret j | j <- [1 .. n - 1]])
  driftAt i = pres VU.! i - pres VU.! max 0 (i - win)
  st i = case classify th (driftAt i) of
    MBull -> 0
    MSw -> 1
    MBear -> 2

-- | closes-based convenience: classify from the given series and run
-- the chain (exact full-series classification)
markovSeries :: MarkovParams -> [Double] -> [Double]
markovSeries p xs
  | n == 0 = []
  | n < 2 = replicate (recordSize * n) nan
  | otherwise = markovSeriesStates p (statesOf p xs)
  where
  n = length xs

-- | flat 15 doubles per bar (see module header), NaN warm-up on BSBP.
-- `states` holds one state per bar of the slice (index 0 unused dummy,
-- states from index 1 on); the chain (counts, surprise, lambda) starts
-- from a uniform prior at slice bar 1
markovSeriesStates :: MarkovParams -> VU.Vector Int -> [Double]
markovSeriesStates p0 sts
  | n == 0 = []
  | n < 2 = replicate (recordSize * n) nan
  | otherwise = runST compute
  where
  p = sanitizeParams p0
  n = VU.length sts
  win = mpWin p
  swin = mpSurpWin p

  -- state of slice bar i (>= 1)
  st :: Int -> Int
  st i = sts VU.! i

  compute :: forall s. ST s [Double]
  compute = do
    out <- VUM.new (recordSize * n)
    forM_ [0 .. recordSize * n - 1] $ \j -> VUM.write out j nan
    bsbp <- newArray (0, 2) 0 :: ST s (STUArray s Int Int)
    cw <- newArray (0, 8) 0.0 :: ST s (STUArray s Int Double)
    tprev <- newArray (0, 8) (1 / 3) :: ST s (STUArray s Int Double)
    ring <- newArray (0, swin - 1) 0.0 :: ST s (STUArray s Int Double)
    -- bar 1's state seeds the BSBP counts (the loop below starts at bar
    -- 2, where the first transition exists); without it the window
    -- ending exactly at i == win would hold win-1 states
    c1 <- readArray bsbp (st 1)
    writeArray bsbp (st 1) (c1 + 1)
    let go :: Int -> Double -> Int -> ST s ()
        go !i !surpSum !surpCnt
          | i >= n = pure ()
          | otherwise = do
              let a = st (i - 1)
                  b = st i
              tp <- readArray tprev (a * 3 + b)
              -- floor the scored probability: a row can be deterministic
              -- ([0,1,0]) and reality may still pick the zero cell; an
              -- infinite log-loss would poison the rolling-sum ring with
              -- inf (inf - inf = NaN on slot eviction)
              let surprise = negate (log (max tp 1e-12))
              -- rolling mean over the last swin surprises (this one
              -- included); the ring slot being overwritten leaves the sum
              rslot <- readArray ring (surpCnt `mod` swin)
              let filled = surpCnt >= swin
                  surpSum' = surpSum + surprise - (if filled then rslot else 0)
                  sMean = surpSum' / fromIntegral (min swin (surpCnt + 1))
                  lam = mpLamLo p + (mpLamHi p - mpLamLo p) * exp (negate (mpK p) * max 0 (sMean - mpSRef p))
              writeArray ring (surpCnt `mod` swin) surprise
              -- decayed count update on the realized cell
              forM_ [0 .. 8] $ \j -> do
                cj <- readArray cw j
                writeArray cw j (cj * lam)
              writeArray cw (a * 3 + b) . (+ 1) =<< readArray cw (a * 3 + b)
              -- row-normalize into tprev (a row is never all-zero after +1,
              -- but keep the guard for safety)
              forM_ [0 .. 2] $ \r -> do
                rs <- sum <$> mapM (readArray cw . (r * 3 +)) [0 .. 2]
                forM_ [0 .. 2] $ \c -> do
                  cell <- readArray cw (r * 3 + c)
                  writeArray tprev (r * 3 + c) (if rs > 0 then cell / rs else 1 / 3)
              -- BSBP slide: add this bar's state, drop the one win bars ago
              cb <- readArray bsbp b
              writeArray bsbp b (cb + 1)
              when (i - win >= 1) $ do
                let old = st (i - win)
                co <- readArray bsbp old
                writeArray bsbp old (co - 1)
              let o = recordSize * i
              when (i >= win) $ do
                forM_ [0 .. 2] $ \r -> do
                  c <- readArray bsbp r
                  VUM.write out (o + r) (fromIntegral c / fromIntegral win)
              forM_ [0 .. 8] $ \j -> do
                tp <- readArray tprev j
                VUM.write out (o + 3 + j) tp
              VUM.write out (o + 12) surprise
              VUM.write out (o + 13) sMean
              VUM.write out (o + 14) lam
              go (i + 1) surpSum' (surpCnt + 1)
    go 2 0.0 0
    VU.toList <$> VU.unsafeFreeze out
