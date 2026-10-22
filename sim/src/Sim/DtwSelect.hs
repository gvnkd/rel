{-# LANGUAGE BangPatterns #-}

-- Second pass over a DTW scan: pick the best set of matched B chunks to
-- reconstruct signal A. Three criteria, all cost terms:
--   1. prefer X (time-warp) modification over Y (amplitude) — chunks whose
--      residual Y error beyond the best affine map is small are cheaper;
--   2. a newly stitched chunk should continue the periodic pattern of the
--      X-scale factors of the chunks selected so far (LS sinus fit on
--      (A-position, scaleX), prediction residual is a cost term);
--   3. overlapping/neighbouring matches are chained into one stitched
--      segment when their A ranges are within `seGap` samples, and every
--      stitch is re-validated: banded DTW of the union A range against the
--      union B span must stay within `seTol` x the median match distance,
--      otherwise the next-best chunk in the group is tried.
-- Reconstruction maps raw B closes through the stitched warp path with
-- per-chunk Y offsets (chunk k+1 is shifted so its first value lands on
-- chunk k's last); A positions not covered by any segment forward-fill
-- the last emitted value.
module Sim.DtwSelect
  ( SelParams (..)
  , defaultSelParams
  , Chunk (..)
  , Segment (..)
  , Rebuild (..)
  , enrichChunks
  , groupChunks
  , selectSegments
  , rebuildCloses
  , selectRebuild
  , periodicDev
  ) where

import Control.Monad.ST (runST)
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Sim.Dtw (Prefix, dtwPathFree, pfAt, pfLen, stepCost, subseqAt, winMean, winStd, zslice, zwin)

data SelParams = SelParams
  { seGap :: !Int     -- max A gap (samples) that still chains two chunks
  , seTol :: !Double  -- stitched-dist acceptance: <= seTol * median match dist
  , seWy  :: !Double  -- weight of the Y-deformation term
  , seWp  :: !Double  -- weight of the periodic scaleX-deviation term
  , seMaxSpan :: !Int -- stitched segment span cap (A samples); validation of
                      -- a bigger union is O(span*span) memory and the
                      -- merging gain saturates
  } deriving (Show)

defaultSelParams :: SelParams
defaultSelParams = SelParams 0 1.5 1.0 0.5 4096

-- one enriched scan match
data Chunk = Chunk
  { chA0      :: !Int     -- [a0, a1) in A
  , chA1      :: !Int
  , chB0      :: !Int     -- [b0, b1) absolute in B (warp-path span)
  , chB1      :: !Int
  , chJ       :: !Int     -- recorded B match start (meta output)
  , chDist    :: !Double  -- recorded per-step z-distance
  , chScaleX  :: !Double  -- spanB / win
  , chYDeform :: !Double  -- RMS residual after the best affine Y map
  } deriving (Show)

-- a stitched run of chunks accepted as one reconstruction segment; the
-- a->b mapping is NOT stored — rebuildCloses recomputes the stitched path
-- deterministically for the few accepted segments
data Segment = Segment
  { sgA0      :: !Int
  , sgA1      :: !Int
  , sgB0      :: !Int
  , sgB1      :: !Int
  , sgDist    :: !Double  -- re-validated stitched per-step distance
  , sgScaleX  :: !Double
  , sgYDeform :: !Double
  , sgChunks  :: [Chunk]  -- constituents in A order (meta output)
  } deriving (Show)

data Rebuild = Rebuild
  { rbCloses  :: !(VU.Vector Double) -- length = |A|, gap-forward-filled
  , rbSegs    :: ![Segment]
  , rbCovered :: !Int
  } deriving (Show)

-- re-run the banded free-end match for every recorded match and extract
-- the warp profile: B span (aggregate X scale) and the affine Y residual
-- along the path (the Y modification the scan's z-norm does NOT factor
-- out). Drops unmatched (j<0) and out-of-range records.
enrichChunks :: Prefix -> Prefix -> Int -> Int -> [(Int, Int, Double)] -> [Chunk]
enrichChunks pfa pfb win band ms =
  [ ch | (i, j, d) <- ms, j >= 0, Just ch <- [mkChunk i j d] ]
  where
  mkChunk i j d
    | i < 0 || win <= 0 || i + win > pfLen pfa || j + win > pfLen pfb = Nothing
    | otherwise = case subseqAt (zslice pfa i win) pfb j band of
        Just (_, path@(_ : _)) ->
          let bs = map snd path
              b0 = j + minimum bs
              b1 = j + maximum bs + 1
              qa = zslice pfa i win
              segLen = min (win + 2 * max 1 band) (pfLen pfb - j)
              qz = zwin pfb j win segLen
              pairs = [ (qa VU.! a, qz VU.! b) | (a, b) <- path ]
              (g, c) = affineFit pairs
              res = sqrt
                  ( sum [ let r = b - g * a - c in r * r | (a, b) <- pairs ]
                    / fromIntegral (length pairs) )
          in Just Chunk
               { chA0 = i
               , chA1 = i + win
               , chB0 = b0
               , chB1 = b1
               , chJ = j
               , chDist = d
               , chScaleX = fromIntegral (b1 - b0) / fromIntegral win
               , chYDeform = res
               }
        _ -> Nothing

-- least-squares b ~ g*a + c over the path pairs
affineFit :: [(Double, Double)] -> (Double, Double)
affineFit pts
  | n == 0 = (1, 0)
  | den < 1e-12 = (1, sb / fromIntegral n)
  | otherwise = (g, c)
  where
  n = length pts
  sa = sum (map fst pts)
  sb = sum (map snd pts)
  saa = sum [ a * a | (a, _) <- pts ]
  sab = sum [ a * b | (a, b) <- pts ]
  den = fromIntegral n * saa - sa * sa
  g = (fromIntegral n * sab - sa * sb) / den
  c = (sb - g * sa) / fromIntegral n

-- chain chunks into groups while the next A start is within `gap` of the
-- group's current A end (criterion 3: overlap/nearby matches form one
-- candidate segment). Groups come out in A order.
groupChunks :: Int -> [Chunk] -> [[Chunk]]
groupChunks gap = map (sortOn chA0) . reverse . foldl' step []
  where
  step [] c = [[c]]
  step (g : gs) c
    | chA0 c <= end g + gap = (c : g) : gs
    | otherwise = [c] : g : gs
  end g = maximum (map chA1 g)

-- greedy left-to-right segment selection. A group first tries to extend
-- the previous segment; when that stops validating (or hits the span
-- cap), the remaining chunks seed a fresh segment, and so on until the
-- group is exhausted. Extension is positional: only chunks whose A range
-- starts within seGap of the segment end are stitch candidates (a
-- handful per step, not the whole group — groups over a densely matched
-- region can hold tens of thousands of chunks). Each stitch re-validates
-- the union A range against the union B span (banded DTW) and is
-- accepted only within seTol * dMed; the lowest-cost candidate —
-- dist + wy*Y-deformation + wp*periodic-deviation — wins.
selectSegments :: SelParams -> Prefix -> Prefix -> Int -> Double -> [[Chunk]] -> [Segment]
selectSegments se pfa pfb band dMed = foldl' try []
  where
  try segs [] = segs
  try segs g =
    let ordered = sortOn (\c -> (chA0 c, chDist c)) g
    in case segs of
      [] -> seedLoop segs ordered
      (p : _) ->
        let grown = extend (histOf segs) p ordered
        in if sgA1 grown == sgA1 p && length (sgChunks grown) == length (sgChunks p)
             then seedLoop segs ordered
             else seedLoop (grown : segs) [ d | d <- ordered, chA1 d > sgA1 grown ]
  -- consume a group (or its remainder) by repeatedly seeding a fresh
  -- segment from the EARLIEST chunk left and extending it positionally
  seedLoop segs [] = segs
  seedLoop segs cand = case cand of
    [] -> segs
    (seed : _) ->
      let seg = extend (histOf segs) (singleton seed) (candidatesAfter cand seed)
      in seedLoop (seg : segs) [ d | d <- cand, chA1 d > sgA1 seg ]
  histOf segs = [ (center s, sgScaleX s) | s <- reverse segs ]
  singleton c = Segment
    { sgA0 = chA0 c
    , sgA1 = chA1 c
    , sgB0 = chB0 c
    , sgB1 = chB1 c
    , sgDist = chDist c
    , sgScaleX = chScaleX c
    , sgYDeform = chYDeform c
    , sgChunks = [c]
    }
  candidatesAfter ordered seed = [ c | c <- ordered, chA1 c > chA1 seed ]
  minBy :: Ord b => (a -> b) -> [a] -> a
  minBy _ [] = error "minBy: empty"
  minBy f (x : xs) = foldl' (\a b -> if f b < f a then b else a) x xs
  extend hist cur cand = case valids of
    [] -> cur
    _  ->
      let (_, c) = minBy fst valids
          Just (_, newSeg) = stitchOne cur hist c
      in extend (hist ++ [ (center newSeg, sgScaleX newSeg) ]) newSeg
                [ d | d <- cand, chA1 d > sgA1 newSeg ]
    where
    -- positional chaining: only chunks whose A range starts within gap of
    -- the segment end can stitch (a handful per step — NOT the whole
    -- group, which can be tens of thousands of chunks over a matched
    -- region). The first non-validating neighborhood ends the segment.
    near = [ c | c <- cand, chA1 c > sgA1 cur, chA0 c <= sgA1 cur + seGap se ]
    valids = [ (cost, c) | c <- take 16 near, Just (cost, _) <- [stitchOne cur hist c] ]
  center s = fromIntegral (sgA0 s + sgA1 s) / 2
  stitchOne p hist c
    | chA1 c <= sgA0 p = Nothing -- entirely left of the segment: no extension
    | max (sgA1 p) (chA1 c) - sgA0 p > seMaxSpan se = Nothing
    | otherwise =
        let a0 = sgA0 p
            a1 = max (sgA1 p) (chA1 c)
            b0 = min (sgB0 p) (chB0 c)
            b1 = max (sgB1 p) (chB1 c)
            spanA = a1 - a0
            spanB = b1 - b0
            -- a stitch maps the union A range onto the union B span; the
            -- banded path can only ever use spanA + 2*band of B (the free
            -- end would silently match the first columns of an absurdly
            -- long span), so unions wider than that are meaningless
            saneSpan = abs (spanB - spanA) <= 2 * band
        in if spanA < 1 || spanB < 1 || not saneSpan
                || a1 > pfLen pfa || b1 > pfLen pfb
             then Nothing
             else
               let q = zslice pfa a0 spanA
                   statsLen = min spanA spanB
                   segz = zwin pfb b0 statsLen spanB
                   (cost, path) = dtwPathFree q segz band
                   dist = stepCost (cost, path)
               in if null path || isBad dist || dist > seTol se * dMed
                    then Nothing
                    else
                      let pairs = [ (q VU.! a, segz VU.! b) | (a, b) <- path ]
                          (g, off) = affineFit pairs
                          yd = sqrt
                             ( sum [ let r = b - g * a - off in r * r | (a, b) <- pairs ]
                               / fromIntegral (length pairs) )
                          dev = seWp se * periodicDev hist (centerOf c) (chScaleX c)
                          newSeg = p
                            { sgA1 = a1
                            , sgB0 = b0
                            , sgB1 = b1
                            , sgDist = dist
                            , sgScaleX = fromIntegral spanB / fromIntegral spanA
                            , sgYDeform = yd
                            , sgChunks = insertChunk c (sgChunks p)
                            }
                      in Just (dist + seWy se * yd + dev, newSeg)
  centerOf c = fromIntegral (chA0 c + chA1 c) / 2
  -- keep sgChunks in A order (c may overlap or extend past the tail)
  insertChunk c cs = sortOn chA0 (c : cs)

isBad :: Double -> Bool
isBad x = isNaN x || isInfinite x

-- deviation of a candidate scaleX from the LS sinus fit over the history
-- of (position, scaleX) of already-selected segments; 0 with fewer than 3
-- history points (no pattern to continue yet)
periodicDev :: [(Double, Double)] -> Double -> Double -> Double
periodicDev hist x v = case hist of
  ((tH, _) : _) | length hist >= 3 ->
    let meanV = sum (map snd hist) / fromIntegral (length hist)
        span = x - tH
    in if span < 1
         then abs (v - meanV)
         else abs (bestFit span meanV [ (c - tH, s) | (c, s) <- hist ] x - v)
  _ -> 0

-- predictor from the best (lowest SSE) single-harmonic LS fit over the
-- period grid; the constant mean when nothing fits
bestFit :: Double -> Double -> [(Double, Double)] -> Double -> Double
bestFit span meanV pts = case best of
  Just (_, f) -> f
  Nothing     -> const meanV
  where
  scored = [ (sse, predFn)
           | f <- grid
           , Just predFn <- [fitAt pts f]
           , let sse = sum [ let r = s - predFn t in r * r | (t, s) <- pts ] ]
  best = case scored of
    [] -> Nothing
    (p0 : rest) -> Just (foldl' pick p0 rest)
  pick acc@(sa, _) c@(sc, _) = if sc < sa then c else acc
  -- periods (in A samples) doubling from 8 to 4096, restricted to the
  -- observable span
  grid = [ 1 / fromIntegral p
         | k <- [3 .. 12 :: Int]
         , let p = (2 :: Int) ^ k
         , fromIntegral p <= 4 * (span + 1) ]

-- LS fit of [1, cos wt, sin wt] by 3x3 normal equations
fitAt :: [(Double, Double)] -> Double -> Maybe (Double -> Double)
fitAt pts f =
  let w = 2 * pi * f
      cs = [ cos (w * t) | (t, _) <- pts ]
      sn = [ sin (w * t) | (t, _) <- pts ]
      n = length pts
      a11 = fromIntegral n
      a12 = sum cs
      a13 = sum sn
      a22 = sum [ c * c | c <- cs ]
      a23 = sum [ c * s | (c, s) <- zip cs sn ]
      a33 = sum [ s * s | s <- sn ]
      b1 = sum (map snd pts)
      b2 = sum [ c * y | (c, y) <- zip cs (map snd pts) ]
      b3 = sum [ s * y | (s, y) <- zip sn (map snd pts) ]
  in case solve3 [ [a11, a12, a13], [a12, a22, a23], [a13, a23, a33] ] [b1, b2, b3] of
       Just [c0, c1, c2] -> Just (\t -> c0 + c1 * cos (w * t) + c2 * sin (w * t))
       _ -> Nothing

-- 3x3 linear solve by Gaussian elimination with partial pivoting
solve3 :: [[Double]] -> [Double] -> Maybe [Double]
solve3 a b
  | length a /= 3 || any ((/= 3) . length) a || length b /= 3 = Nothing
  | otherwise = do
      m <- eliminate (zipWith (++) a (map (: []) b)) 0
      -- forward elimination leaves an upper triangle; back-substitute
      let x2 = (m !! 2) !! 3 / (m !! 2) !! 2
          x1 = ((m !! 1) !! 3 - (m !! 1) !! 2 * x2) / (m !! 1) !! 1
          x0 = ((m !! 0) !! 3 - (m !! 0) !! 1 * x1 - (m !! 0) !! 2 * x2) / (m !! 0) !! 0
      [x0, x1, x2] `seq` Just [x0, x1, x2]
  where
  eliminate m 3 = Just m
  eliminate m k = do
    let col = [ abs ((m !! i) !! k) | i <- [k .. 2] ]
    if maximum col < 1e-12
      then Nothing
      else do
        let p = maxIndex col + k
            m1 = swapRows k p m
            pivot = (m1 !! k) !! k
            m2 = [ if ri == k then row
                   else let f = (row !! k) / pivot
                        in zipWith (\x y -> x - f * y) row (m1 !! k)
                 | (ri, row) <- zip [0 :: Int ..] m1 ]
        eliminate m2 (k + 1)
  maxIndex = fst . foldl' (\(bi, bv) (i, v) -> if v > bv then (i :: Int, v) else (bi, bv)) (0, 0) . zip [0 :: Int ..]
  swapRows i j m
    | i == j = m
    | otherwise = [ m !! if ri == i then j else if ri == j then i else ri | ri <- [0 .. 2] ]

-- compose the reconstructed A-length close series. Each segment maps its
-- A range through the stitched warp path (re-run deterministically, same
-- call as the stitch validation). Values get a per-chunk AFFINE Y map so
-- the reconstruction matches A's local price SCALE, not just its mean:
-- value_k = base + g*(rawB_k - rawB_lo) with g = stdA/stdB over the
-- chunk's range (the z-norm equivalence the drill chart shows). Chunk 1
-- anchors its range mean at A's local mean; chunk k+1 starts where chunk
-- k's last emitted value landed (Sergey's continuity rule) but keeps its
-- own scale. Positions in inter-chunk gaps use the next chunk's affine
-- (smooth backward extrapolation). Positions outside every segment
-- forward-fill the last emitted value; a leading gap backward-fills the
-- first segment's first value.
rebuildCloses :: Prefix -> Prefix -> Int -> [Segment] -> VU.Vector Double
rebuildCloses _ _ _ [] = VU.empty
rebuildCloses pfa pfb band segs0 = runST $ do
  out <- VUM.replicate na (0 / 0)
  mapM_ (writeSeg out) segs0
  firstV <- findFirst out
  _ <- foldFill out (maybe 0 id firstV) 0
  VU.unsafeFreeze out
  where
  na = pfLen pfa
  rawB = pfAt pfb
  writeSeg out s = do
    let a0 = sgA0 s
        spanA = sgA1 s - sgA0 s
        m = segMap s
        raw k = rawB (m VU.! k)
        ranges = chunkRanges s
        affs = bases s raw ranges
        n = length affs
        val k (base, g, rlo, _) = base + g * (raw k - rlo)
        hiOf (_, _, _, hi) = hi
        go k i
          | k >= spanA = pure ()
          | otherwise = do
              let i' = advance i k
              VUM.write out (a0 + k) (val k (affs !! i'))
              go (k + 1) i'
          where
          advance j kk
            | j + 1 < n && kk >= hiOf (affs !! j) = advance (j + 1) kk
            | otherwise = j
    case affs of
      [] -> pure ()
      _ -> go 0 0
  -- per-chunk in-seg A ranges [lo, hi), ordered, clamped to the segment
  chunkRanges s =
    [ (lo, hi)
    | c <- sgChunks s
    , let lo = max (sgA0 s) (chA0 c) - sgA0 s
          hi = min (sgA1 s) (chA1 c) - sgA0 s
    , hi > lo ]
  -- per-chunk (base, g, rawAtLo, hi): base = emitted value at the chunk's
  -- first position, g = stdA/stdB over the chunk's range
  bases s raw ranges = snd (foldl' step (Nothing, []) ranges)
    where
    step (prev, acc) (loC, hiC) =
      let n = hiC - loC
          muA = winMean pfa (sgA0 s + loC) n
          sdA = winStd pfa (sgA0 s + loC) n
          muB = sum [ raw k | k <- [loC .. hiC - 1] ] / fromIntegral n
          sdB = sqrt (sum [ (raw k - muB) * (raw k - muB) | k <- [loC .. hiC - 1] ] / fromIntegral n)
          g = sdA / max 1e-12 sdB
          base = case prev of
            Nothing -> muA - g * (muB - raw loC)
            Just (bP, gP, rloP, hiP) -> bP + gP * (raw (hiP - 1) - rloP)
          cur = (base, g, raw loC, hiC)
      in (Just cur, acc ++ [cur])
  -- a in [sgA0, sgA1) -> absolute b via the re-run stitched path
  segMap s =
    let spanA = sgA1 s - sgA0 s
        spanB = sgB1 s - sgB0 s
        q = zslice pfa (sgA0 s) spanA
        segz = zwin pfb (sgB0 s) (min spanA spanB) spanB
        (_, path) = dtwPathFree q segz band
        sums = VU.accumulate (+) (VU.replicate spanA 0) (VU.fromList [ (a, sgB0 s + b) | (a, b) <- path ])
        cnts = VU.accumulate (+) (VU.replicate spanA 0) (VU.fromList [ (a, 1 :: Int) | (a, _) <- path ])
    in VU.zipWith (\sm cn -> if cn == 0 then -1 else sm `div` cn) sums cnts
  findFirst out = go 0
    where
    go i
      | i >= na = pure Nothing
      | otherwise = do
          v <- VUM.read out i
          if isBad v then go (i + 1) else pure (Just v)
  foldFill out cur i
    | i >= na = pure cur
    | otherwise = do
        v <- VUM.read out i
        if isBad v
          then VUM.write out i cur >> foldFill out cur (i + 1)
          else foldFill out v (i + 1)

-- enrich -> group -> select -> rebuild in one call
selectRebuild :: SelParams -> Prefix -> Prefix -> Int -> Int -> [(Int, Int, Double)] -> Rebuild
selectRebuild se pfa pfb win band ms =
  let chunks = enrichChunks pfa pfb win band ms
      dMed = median (map chDist chunks)
      groups = groupChunks (seGap se) chunks
      segs = reverse (selectSegments se pfa pfb band dMed groups)
  in Rebuild (rebuildCloses pfa pfb band segs) segs (coveredUnion segs)

-- union length of all segment A ranges (segments may overlap — singleton
-- re-seeds after a rejected stitch often tile the same neighborhood)
coveredUnion :: [Segment] -> Int
coveredUnion segs = go 0 (sortOn sgA0 segs)
  where
  go _ [] = 0
  go lo (s : rest) =
    let a = max (sgA0 s) lo
        b = max (sgA1 s) a
    in (b - a) + go b rest

median :: [Double] -> Double
median xs = case sortOn id xs of
  [] -> 0
  ys -> let n = length ys in ys !! ((n - 1) `div` 2)
