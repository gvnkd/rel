module Main (main) where

import Data.Bits (shiftL, shiftR, xor)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort, sortOn)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Sim.Engine
import Sim.Entropy (cecpOf, permEntropy, rollingCECP, rollingPermEntropy)
import qualified Data.Vector.Unboxed as VU
import Sim.Fft (corr, corrWV, fftApprox, fftBest, fftForecast, gram, gramW, rampMass, refinePeaks, solveLS)
import Sim.Gen
import Sim.GenH (runSimH)
import Sim.Csv (csvText, tickRow)
import Sim.Lvl (LvlEv (..), LvlSeg (..), LvlTr, emptyTr, trCloseAll, trConsume, trFromBook, trRest)
import qualified Sim.ML.Data as MLD
import qualified Sim.ML.Model as MLM
import qualified Sim.ML.Train as MLT
import Sim.Resume (parseState, stateText)
import Sim.Sinus (runSimSinus, sinusCandlesFrom, sinusPriceAt, sinusVolAt)
import Sim.TickGen (runSimTicks)
import Sim.TickGenH (runSimTicksH)
import System.Exit (exitFailure, exitSuccess)
import Sim.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Random.SplitMix (mkSMGen)
import Test.QuickCheck hiding (Result)
import Test.QuickCheck (Result)

-- order flows, mirroring the invariants the GDScript suite checks
newtype Flow = Flow [Order]
  deriving (Show)

genSide :: Gen Side
genSide = elements [Bid, Ask]

genPrice :: Gen Price
genPrice = Price <$> chooseInt (0, 100)

genVol :: Gen Vol
genVol = fromJust . mkVol <$> chooseInt (1, 20)

instance Arbitrary Flow where
  arbitrary = Flow <$> listOf1 (Order <$> genSide <*> genPrice <*> genVol)

buildBook :: Flow -> Book
buildBook (Flow orders) = foldl' (flip placeOrder) emptyBook orders

prop_no_crossed_book :: Flow -> Property
prop_no_crossed_book f =
  let book = fst (processOrders (buildBook f))
  in classify (Map.null (bookBids book) || Map.null (bookAsks book)) "one side empty"
     $ case (bestBid book, bestAsk book) of
         (Just (bp, _), Just (ap, _)) -> property (bp < ap)
         _                            -> property True

prop_positive_volumes :: Flow -> Bool
prop_positive_volumes f =
  let book = fst (processOrders (buildBook f))
      vols = Map.elems (bookBids book) ++ Map.elems (bookAsks book)
  in all (\v -> unVol v > 0) vols

prop_trades_at_ask_prices :: Flow -> Bool
prop_trades_at_ask_prices f =
  let book0 = buildBook f
      askPrices = Map.keysSet (bookAsks book0)
      (_, trades) = processOrders book0
  in all (\t -> Set.member (tradePrice t) askPrices) trades

prop_trade_volume_conserved :: Flow -> Bool
prop_trade_volume_conserved (Flow orders) =
  let book0 = foldl' (flip placeOrder) emptyBook orders
      placed = sum (map (unVol . orderVol) orders)
      (_, trades) = processOrders book0
      traded = sum (map (unVol . tradeVol) trades)
  in all (> 0) (map (unVol . tradeVol) trades) && traded <= placed

prop_candle_invariants :: [Int] -> Property
prop_candle_invariants ps0 =
  let ps = map Price (filter (/= 0) ps0)
      c = foldl' (\c p -> updateCandle p 1 c) emptyCandle ps
      mFirst = listToMaybe ps
      mLast = listToMaybe (reverse ps)
  in case (mFirst, mLast) of
        (Just o, Just cl) -> property $
             candleLow c <= min (candleOpen c) (candleClose c)
          && max (candleOpen c) (candleClose c) <= candleHigh c
          && candleOpen c == o
          && candleClose c == cl
          && candleVol c == length ps
        _ -> property True
prop_sim_produces_valid_candles :: Word64 -> Property
prop_sim_produces_valid_candles seed =
  let cs = runSim defaultParams { spSeed = seed, spCandles = 50, spOrdersLo = 100, spOrdersHi = 100 }
  in not (null cs) ==> all validCandle cs

prop_sim_valid_with_options :: Word64 -> Bool -> Property
prop_sim_valid_with_options seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 100
        , spOrdersHi = 100
        , spClampRest = clamp
        }
      cs = runSim sp
  in not (null cs) ==> all validCandle cs

prop_sim_valid_with_order_range :: Word64 -> Bool -> Property
prop_sim_valid_with_order_range seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 60
        , spOrdersHi = 140
        , spClampRest = clamp
        }
      cs = runSim sp
  in not (null cs) ==> all validCandle cs

-- the book must never stay empty: a run with plenty of orders per candle
-- always produces candles
prop_sim_no_dead_market :: Word64 -> Bool -> Property
prop_sim_no_dead_market seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 20
        , spOrdersLo = 200
        , spOrdersHi = 200
        , spClampRest = clamp
        }
  in property (length (runSim sp) == 20)

-- percent offsets: prices stay strictly positive and candles stay valid,
-- even with a tick-range wider than 10000 bp (the ask floored at 1 tick
-- inside movePct carries this)
prop_sim_percent_offsets_positive :: Word64 -> Bool -> Property
prop_sim_percent_offsets_positive seed clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 100
        , spOrdersHi = 100
        , spTickRange = 20000
        , spClampRest = clamp
        , spPriceOffset = OffsetPercent
        }
      cs = runSim sp
  in not (null cs) ==> all (\c -> validCandle c && unPrice (candleLow c) > 0) cs

-- zero floor: with arithmetic offsets and an enormous tick-range the
-- book walks to zero; with the floor on, agents refuse non-positive
-- quotes so no trade ever prints at or below zero
prop_sim_zero_floor :: Word64 -> Property
prop_sim_zero_floor seed =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 50
        , spOrdersLo = 100
        , spOrdersHi = 100
        , spTickRange = 100000
        , spZeroFloor = True
        }
      cs = runSim sp
  in not (null cs) ==> all (\c -> validCandle c && unPrice (candleLow c) > 0) cs

-- percent mode + floor: tick export compacts to the same candles as runSim
prop_ticks_candles_match_runSim_percent :: Word64 -> SimMode -> Bool -> Property
prop_ticks_candles_match_runSim_percent seed mode clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 30
        , spOrdersLo = 50
        , spOrdersHi = 50
        , spMode = mode
        , spClampRest = clamp
        , spPriceOffset = OffsetPercent
        , spZeroFloor = True
        }
      (cs, _ts, _ps, _ev) = runSimTicks sp
  in property $ cs == runSim sp

prop_ticks_candles_match_runSim :: Word64 -> SimMode -> Bool -> Property
prop_ticks_candles_match_runSim seed mode clamp =
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 30
        , spOrdersLo = 50
        , spOrdersHi = 50
        , spMode = mode
        , spClampRest = clamp
        }
      (cs, _ts, _ps, _ev) = runSimTicks sp
  in property $ cs == runSim sp

prop_ticks_book_snapshots :: Word64 -> SimMode -> Property
prop_ticks_book_snapshots seed mode =
  let sp = defaultParams { spSeed = seed, spCandles = 20, spOrdersLo = 50, spOrdersHi = 50, spMode = mode, spBookDepth = 10 }
      (cs, ts, _ps, _ev) = runSimTicks sp
      lastVol i = case [tickVol t | t <- ts, tickCandle t == i] of
        [] -> -1
        vs -> last vs
  in counterexample "candle count" (length [ tickCandle t | t <- ts, tickVol t == 0 ] >= length cs)
     .&&. counterexample "marker last" (all (\i -> lastVol i == 0) [0 .. length cs - 1])
     .&&. counterexample "sorted levels" (all sortedLevels ts)

sortedLevels :: Tick -> Bool
sortedLevels t =
  let bidPs = map (unPrice . fst) (tickBids t)
      askPs = map (unPrice . fst) (tickAsks t)
      vols  = map (unVol . snd) (tickBids t ++ tickAsks t)
  in bidPs == reverse (sort bidPs)
     && askPs == sort askPs
     && length bidPs <= 10 && length askPs <= 10
     && all (> 0) vols

-- level-lifetime tracker (Sim.Lvl): events mirroring what the tick
-- drivers observe — Rest opens/bumps an open level, Match consumes the
-- same amount from the best bid and the best ask level
data LvlFlowEv = LRest Int Side Price Int | LMatch Int Price Price Int
  deriving (Show)

newtype LvlFlow = LvlFlow [LvlFlowEv]
  deriving (Show)

instance Arbitrary LvlFlow where
  arbitrary = LvlFlow <$> listOf1 genEv
    where
      genEv = oneof
        [ LRest <$> chooseInt (0, 20) <*> genSide <*> genPrice <*> chooseInt (1, 20)
        , LMatch <$> chooseInt (0, 20) <*> genPrice <*> genPrice <*> chooseInt (1, 20)
        ]

evCi :: LvlFlowEv -> Int
evCi (LRest ci _ _ _)  = ci
evCi (LMatch ci _ _ _) = ci

-- drive the Sim.Lvl tracker over a flow; open levels flush at the end
-- with c1 = the flush index, same as the tick drivers do
lvlTrack :: [LvlFlowEv] -> LvlTr -> Int -> [(LvlSeg, [LvlEv])]
lvlTrack evs tr0 flushCi = go evs tr0 []
  where
    go [] tr acc = reverse acc ++ trCloseAll flushCi tr
    go (e : rest) tr acc = case e of
      LRest ci side p v ->
        go rest (trRest ci side p v tr) acc
      LMatch ci bp ap v ->
        let (tr', pairs) = trConsume ci bp ap v tr
        in go rest tr' (reverse pairs ++ acc)

-- reference model replicating the tracker exactly, including the
-- close-time event-range assignment (segment ev0/evn must match)
lvlModel :: [LvlFlowEv] -> Int -> [(LvlSeg, [LvlEv])]
lvlModel evs flushCi =
  let (m, ne, closed) = foldl' step (Map.empty, 0, []) evs
      open = goOpen ne
        -- toAscList sorts by (Side, Price): Bid group first, then Ask —
        -- the same flush order as trCloseAll
        [ (side, p, x) | ((side, p), x) <- Map.toAscList m ]
  in closed ++ open
  where
    step (m, ne, closed) e = case e of
      LRest ci side p v -> case Map.lookup (side, p) m of
        Nothing ->
          ( Map.insert (side, p) (v, v, ci, [LvlEv ci side p v]) m
          , ne
          , closed
          )
        Just (c, pk, st, buf) ->
          let c' = c + v
          in ( Map.insert (side, p) (c', max pk c', st, LvlEv ci side p c' : buf) m
             , ne
             , closed
             )
      LMatch ci bp ap v ->
        let (m1, ne1, c1) = eat ci v m ne Bid bp
            (m2, ne2, c2) = eat ci v m1 ne1 Ask ap
        in (m2, ne2, closed ++ c1 ++ c2)
    eat ci v mm ne side p = case Map.lookup (side, p) mm of
      Nothing -> (mm, ne, [])
      Just (c, pk, st, buf)
        | c <= v    ->
            ( Map.delete (side, p) mm
            , ne + length buf
            , [(LvlSeg st ci side p pk ne (length buf), reverse buf)]
            )
        | otherwise ->
            let c' = c - v
            in ( Map.insert (side, p) (c', max pk c', st, LvlEv ci side p c' : buf) mm
               , ne
               , []
               )
    goOpen _ [] = []
    goOpen n ((side, p, (_, pk, st, buf)) : rest) =
      (LvlSeg st flushCi side p pk n (length buf), reverse buf) : goOpen (n + length buf) rest

prop_lvl_tracker_matches_model :: LvlFlow -> Bool
prop_lvl_tracker_matches_model (LvlFlow evs) =
  let flushCi = maximum (map evCi evs)
  in lvlTrack evs emptyTr flushCi == lvlModel evs flushCi

-- segments from a tick run: in-bounds, positive peaks, and per (side,
-- price) lifetimes never overlap (a new one starts only after the
-- previous one closed)
prop_lvl_segments_coherent :: Word64 -> SimMode -> Bool
prop_lvl_segments_coherent seed mode =
  let sp = defaultParams { spSeed = seed, spCandles = 20, spOrdersLo = 50, spOrdersHi = 50, spMode = mode, spBookDepth = 5 }
      (_cs, _ts, ps, _ev) = runSimTicks sp
      ls = map fst ps
      byLevel = Map.fromListWith (++) [ ((lvSide s, lvPx s), [s]) | s <- ls ]
      ordered segs =
        let ss = sortOn (\s -> (lvC0 s, lvC1 s)) segs
        in and (zipWith (\a b -> lvC0 b >= lvC1 a) ss (drop 1 ss))
  in all (\s -> lvC0 s >= 0 && lvC0 s <= lvC1 s && lvC1 s < spCandles sp && lvPeak s > 0) ls
     && all ordered (Map.elems byLevel)

-- each segment's event range covers exactly its records, and the ranges
-- tile the event stream [0, nEv) as a set (list order is close order —
-- the end-of-run flush appends still-open lifetimes opened early)
prop_lvl_events_flat_consistent :: Word64 -> SimMode -> Bool
prop_lvl_events_flat_consistent seed mode =
  let sp = defaultParams { spSeed = seed, spCandles = 20, spOrdersLo = 50, spOrdersHi = 50, spMode = mode, spBookDepth = 5 }
      (_cs, _ts, ps, nEv) = runSimTicks sp
      rangesOk = all (\(s, es) -> lvEvN s == length es) ps
      sorted = sortOn fst [ (lvEv0 s, lvEvN s) | (s, _) <- ps ]
      tile = all (\(a, b) -> fst b == fst a + snd a) (zip sorted (drop 1 sorted))
  in rangesOk && (null sorted || fst (head sorted) == 0)
     && tile
     && sum (map snd sorted) == nEv

instance Arbitrary SimMode where
  arbitrary = elements [ModeCandles, ModeTickTrades, ModeTickSteps]

validCandle :: Candle -> Bool
validCandle c =
  not (isEmptyCandle c)
  && candleLow c <= min (candleOpen c) (candleClose c)
  && max (candleOpen c) (candleClose c) <= candleHigh c

-- an exact harmonic over a power-of-two window must be continued almost
-- exactly (FFT bin hits the frequency, LS refit recovers amplitude/phase)
prop_fft_recovers_sinusoid :: Bool
prop_fft_recovers_sinusoid =
  let n = 256
      f = 1 / 16 :: Double
      sig t = 100 + 3 * cos (2 * pi * f * fromIntegral t)
      series = [sig t | t <- [0 .. n - 1]]
      (_, fc) = fftApprox 0 1 series 0 n 32
      expect = [sig t | t <- [n .. n + 31]]
  in length fc == 32 && maximum (map abs (zipWith (-) fc expect)) < 1e-6

-- same, but the window sits at an offset inside a longer series
prop_fft_recovers_sinusoid_offset :: Bool
prop_fft_recovers_sinusoid_offset =
  let n = 512
      off = 100
      f = 4 / 512 :: Double
      sig t = 50 - 2 * sin (2 * pi * f * fromIntegral t)
      series = [sig t | t <- [0 .. n - 1]]
      (_, fc) = fftApprox 0 1 series off 356 16
      expect = [sig t | t <- [356 .. 371]]
  in length fc == 16 && maximum (map abs (zipWith (-) fc expect)) < 1e-6

-- the closed-form Gram matrix and correlations must agree with a brute
-- force build of the design matrix columns
prop_fft_gram_matches_bruteforce :: Int -> [Int] -> Bool
prop_fft_gram_matches_bruteforce n0 seeds =
  let n = 10 + abs n0 `mod` 50
      k = 1 + length seeds `mod` 3
      freqs = [ (1 + fromIntegral (abs s `mod` 7)) / fromIntegral n | s <- take k seeds ]
      ws = map (2 * pi *) freqs
      ts = map fromIntegral [0 .. n - 1] :: [Double]
      cols = concat [ [map (cos . (wj *)) ts, map (sin . (wj *)) ts] | wj <- ws ]
      brute = [ [sum (zipWith (*) ci cj) | cj <- cols] | ci <- cols ]
      closed = gram n ws
      ys = take n ([ fromIntegral (abs s `mod` 1000) | s <- seeds ] ++ repeat 7)
      bruteC = concat [ [sum (zipWith (*) (map (cos . (wj *)) ts) ys), sum (zipWith (*) (map (sin . (wj *)) ts) ys)] | wj <- ws ]
      closedC = concatMap (\w -> let (c, s) = corr n ys w in [c, s]) ws
      close a b = abs (a - b) < 1e-6
      -- weighted path: same comparison against a brute-force build with
      -- the ramp weights (h = max 1 (n div 8), ramps at both edges)
      h = max 2 (n `div` 128)
      edgeW = 17
      wts = [ 1 + (edgeW - 1) * max 0 (max (1 - fromIntegral t / fromIntegral h) (1 - fromIntegral (n - 1 - t) / fromIntegral h)) | t <- [0 .. n - 1] ]
      colsW = map (zipWith (*) wts) cols
      ysW = zipWith (*) wts ys
      bruteW = [ [sum (zipWith (*) ci cj) | cj <- cols] | ci <- colsW ]
      closedW = gramW n edgeW ws
      bruteCW = concat [ [sum (zipWith (*) (map (cos . (wj *)) ts) ysW), sum (zipWith (*) (map (sin . (wj *)) ts) ysW)] | wj <- ws ]
      closedCW = concatMap (\w -> let (c, s) = corrWV n (VU.fromList ys) edgeW w in [c, s]) ws
      okBase = length closed == length brute && and (zipWith close (concat closed) (concat brute))
        && and (zipWith close closedC bruteC)
      okW = and (zipWith close (concat closedW) (concat bruteW))
        && and (zipWith close closedCW bruteCW)
      maxW = maximum (0 : [ abs (a - b) | (a, b) <- zip (concat closedW) (concat bruteW) ])
      maxCW = maximum (0 : [ abs (a - b) | (a, b) <- zip closedCW bruteCW ])
  in if okBase && okW then True
     else error ("maxW=" ++ show maxW ++ " maxCW=" ++ show maxCW
                 ++ " n=" ++ show n ++ " ws=" ++ show ws
                 ++ " closedW=" ++ show (take 4 (concat closedW))
                 ++ " bruteW=" ++ show (take 4 (concat bruteW)))

-- on a well-conditioned system the solve must satisfy the normal
-- equations (validates the Jacobi eigendecomposition end to end)
prop_fft_solve_residual :: [Int] -> Bool
prop_fft_solve_residual seeds =
  let n = 200
      freqs = [1 / 8, 1 / 20, 3 / 16 :: Double]
      ws = map (2 * pi *) freqs
      ys = take n ([ fromIntegral (abs s `mod` 1000) | s <- seeds ] ++ repeat 0.5)
      g = gram n ws
      b = concatMap (\w -> let (c, s) = corr n ys w in [c, s]) ws
      x = solveLS n 1 ys freqs
      resid = sqrt (sum [ (sum (zipWith (*) (g !! i) x) - b !! i) ^ 2 | i <- [0 .. length b - 1] ])
      bnorm = sqrt (sum [ bi ^ 2 | bi <- b ])
  in bnorm > 0 && resid / bnorm < 1e-6

-- extrapolation stays bounded even at the max wave count: the truncated
-- eigensolve cannot produce the runaway coefficients that shoot the
-- forecast out of the plot
prop_fft_forecast_bounded :: [Int] -> Bool
prop_fft_forecast_bounded seeds =
  let n = 64
      ys = [ fromIntegral (abs s `mod` 100) | s <- take n seeds ]
      (_, fc) = fftApprox (n `div` 4) 1 ys 0 n 16
      ymax = maximum (map abs ys) + 1
  in null fc || maximum (map abs fc) < 10 * ymax

-- the in-window fit must reproduce the window itself
prop_fft_fit_covers_window :: Bool
prop_fft_fit_covers_window =
  let n = 256
      f = 1 / 16 :: Double
      sig t = 100 + 3 * cos (2 * pi * f * fromIntegral t)
      series = [sig t | t <- [0 .. n - 1]]
      (fit, _) = fftApprox 0 1 series 0 n 32
  in length fit == n && maximum (map abs (zipWith (-) fit series)) < 1e-6

-- with waves=1 only the dominant bin is kept: a second, weaker harmonic
-- must be projected out of the fit and forecast (bins are orthogonal
-- over complete cycles)
prop_fft_waves_limit :: Bool
prop_fft_waves_limit =
  let n = 512
      f1 = 32 / 512 :: Double
      f2 = 64 / 512 :: Double
      sig t = 100 + 3 * cos (2 * pi * f1 * fromIntegral t) + 0.3 * cos (2 * pi * f2 * fromIntegral t)
      main t = 100 + 3 * cos (2 * pi * f1 * fromIntegral t)
      series = [sig t | t <- [0 .. n - 1]]
      (fit, fc) = fftApprox 1 1 series 0 n 16
      bad = maximum
        (map abs (zipWith (-) fit (map main [0 .. n - 1]))
           ++ map abs (zipWith (-) fc (map main [n .. n + 15])))
  in bad < 1e-6

-- a window holding a fractional number of cycles must still be fit and
-- continued accurately: bin refinement recovers the true frequency even
-- when the period does not divide the window (snapping alone drifts up
-- to half a cycle out of phase, worst at the edges and in the forecast)
prop_fft_recovers_sinusoid_fractional :: Int -> Int -> Bool
prop_fft_recovers_sinusoid_fractional n0 p0 =
  let n = 80 + abs n0 `mod` 120
      p = 9 + abs p0 `mod` 40
      sig t = 10000 + 100 * sin (2 * pi * fromIntegral t / fromIntegral p) :: Double
      series = take (n + 24) [ fromIntegral (round (sig t)) | t <- [0 ..] ]
      frac = fromIntegral n / fromIntegral p
      off = frac - fromIntegral (floor frac)
      intCyc = off < 0.02 || off > 0.98
      (fit, fc) = fftApprox 1 1 series 0 n 16
      fitErr = maximum (0 : [ abs (f - s) | (f, s) <- zip fit (map sig [0 .. n - 1]) ])
      fcErr = maximum (0 : [ abs (f - s) | (f, s) <- zip fc (map sig [n .. n + 15]) ])
  in intCyc || (fitErr < 5 && fcErr < 5)

-- the same for the sum-of-waves generator signal: both harmonics must be
-- found (non-maximum suppression keeps the strong wave's mainlobe from
-- starving the weaker harmonic of the top-k picks) and tracked
prop_fft_recovers_multiwave_fractional :: Int -> Bool
prop_fft_recovers_multiwave_fractional n0 =
  let n = 80 + abs n0 `mod` 120
      sig t = 10000 + 100 * sin (2 * pi * fromIntegral t / 20) + 50 * sin (2 * pi * fromIntegral t / 10) :: Double
      series = take (n + 24) [ fromIntegral (round (sig t)) | t <- [0 ..] ]
      (fit, fc) = fftApprox 0 1 series 0 n 16
      fitErr = maximum (0 : [ abs (f - s) | (f, s) <- zip fit (map sig [0 .. n - 1]) ])
      fcErr = maximum (0 : [ abs (f - s) | (f, s) <- zip fc (map sig [n .. n + 15]) ])
  in fitErr < 20 && fcErr < 30

-- refinement is local: refined frequencies stay within one window bin of
-- their anchor and never explode on adversarial data
prop_fft_refine_local :: Int -> [Int] -> Bool
prop_fft_refine_local n0 seeds =
  let n = 16 + abs n0 `mod` 100
      ys = VU.fromList (take n ([ fromIntegral (abs s `mod` 1000) | s <- seeds ] ++ repeat 0.5))
      anchors = [ fromIntegral q / fromIntegral n | q <- [1 .. min 4 (n `div` 8)] ]
      refined = refinePeaks n ys (zip anchors anchors)
      close f g = abs (f - g) <= 1 / fromIntegral n + 1e-12
  in length refined == length anchors && and (zipWith close refined anchors)

-- length contract: full window -> (window, predict) points, short window
-- or non-positive predict -> nothing
prop_fft_forecast_length :: Int -> Int -> Int -> Bool
prop_fft_forecast_length total w p =
  let total' = 16 + abs total `mod` 500
      w' = abs w `mod` (total' + 1)
      p' = abs p `mod` 100 + 1
      series = map (sin . fromIntegral) [0 .. total' - 1]
      (fit, fc) = fftApprox 0 1 series 0 w' p'
  in w' < 16 || (length fit == w' && length fc == p')

prop_fft_forecast_invalid :: [Int] -> Bool
prop_fft_forecast_invalid xs =
  let series = map fromIntegral xs
      (f1, c1) = fftApprox 0 1 series 0 100 0
      (f2, c2) = fftApprox 0 1 series 0 5 10
      (f3, c3) = fftApprox 0 1 series 0 100 (-3)
  in null f1 && null c1 && null f2 && null c2 && null f3 && null c3

-- sinus candles are always valid, strictly positive, and carry volume
prop_sinus_candles_valid :: Int -> Int -> Int -> Int -> Bool
prop_sinus_candles_valid period amp waves n =
  let sp = sinusParams period amp waves n
      cs = runSimSinus sp
  in not (null cs)
     && all (\c -> validCandle c
                && unPrice (candleLow c) > 0
                && unPrice (candleHigh c) > 0
                && candleVol c > 0) cs

-- with a single wave the closes repeat with the period (up to the 1-tick
-- quantization of the sin evaluation)
prop_sinus_periodicity :: Int -> Int -> Bool
prop_sinus_periodicity period0 extra =
  let per = 2 + abs period0 `mod` 100
      n = per + abs extra `mod` 100
      sp = (sinusParams period0 500 1 n) { spSinusPeriod = per }
      cs = map (unPrice . candleClose) (runSimSinus sp)
  in and [ abs (cs !! i - cs !! (i + per)) <= 1 | i <- [0 .. n - per - 1] ]

-- closes track the composite formula within the quantization tick; the
-- range stays inside [base - 2*amp, base + 2*amp]
prop_sinus_matches_formula :: Int -> Int -> Int -> Int -> Bool
prop_sinus_matches_formula period amp waves n =
  let sp = sinusParams period amp waves n
      cs = runSimSinus sp
      base = fromIntegral (unPrice (spStartBid sp) + unPrice (spStartAsk sp)) / 2
      ampTicks = min (base / 4) (base * fromIntegral (spSinusAmp sp) / 10000)
      closeOk (i, c) =
        abs (fromIntegral (unPrice (candleClose c)) - sinusPriceAt sp i) <= 1
        && abs (fromIntegral (unPrice (candleClose c)) - base) <= 2 * ampTicks + 1
      volOk (i, c) =
        abs (fromIntegral (candleVol c) - sinusVolAt sp i) <= 1
  in and (map closeOk (zip [0 ..] cs)) && and (map volOk (zip [0 ..] cs))

-- volumes are periodic with the single-wave period and positive
prop_sinus_volumes_periodic :: Int -> Int -> Bool
prop_sinus_volumes_periodic period0 extra =
  let per = 2 + abs period0 `mod` 100
      n = per + abs extra `mod` 100
      sp = (sinusParams period0 500 1 n) { spSinusPeriod = per }
      vs = map candleVol (runSimSinus sp)
  in all (> 0) vs
     && and [ abs (vs !! i - vs !! (i + per)) <= 1 | i <- [0 .. n - per - 1] ]

-- a resumed segment continues the exact sequence: the open of the first
-- resumed candle chains the last close of the previous segment
prop_sinus_segments_join :: Int -> Int -> Bool
prop_sinus_segments_join n0 m0 =
  let a = 1 + abs n0 `mod` 100
      b = 1 + abs m0 `mod` 100
      sp = (sinusParams 17 733 3 (a + b)) { spCandles = a + b }
      whole = runSimSinus sp
      joined = sinusCandlesFrom sp 0 a ++ sinusCandlesFrom sp a b
  in whole == joined

-- the sinus state snapshot round-trips with just params + done
prop_sinus_state_roundtrip :: Int -> Bool
prop_sinus_state_roundtrip n0 =
  let sp = (sinusParams 23 611 2 50) { spCandles = 1 + abs n0 `mod` 100 }
      done = abs n0 `mod` 50
  in case parseState (stateText sp done (initState sp)) of
       Left err -> error err
       Right (sp', done', _gs) -> show sp == show sp' && done == done'

sinusParams :: Int -> Int -> Int -> Int -> SimParams
sinusParams period amp waves n = defaultParams
  { spSinus = True
  , spSinusPeriod = 2 + abs period `mod` 300
  , spSinusAmp = abs amp `mod` 2000
  , spSinusWaves = abs waves `mod` 6
  , spCandles = 1 + abs n `mod` 200
  }

-- a resumed run continues the exact same sequence: two hooked segments
-- (snapshotting the state in between) produce the same candles as one
-- pure runSim of the combined length
prop_resume_candles :: Word64 -> Property
prop_resume_candles seed = ioProperty $ do
  let sp = defaultParams { spSeed = seed, spCandles = 40, spOrdersLo = 80, spOrdersHi = 120 }
  (gs1, cs1) <- runSimH defaultHooks sp { spCandles = 20 } (initState sp)
  (_gs2, cs2) <- runSimH defaultHooks sp { spCandles = 20 } gs1
  pure (cs1 ++ cs2 == runSim sp)

-- the continuation snapshot round-trips: parsing a rendered state text
-- yields the same params, done count, rng, book, anchor, and in-progress
-- candle
prop_state_roundtrip :: Word64 -> Bool -> Property
prop_state_roundtrip seed clamp = ioProperty $ do
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 20
        , spOrdersLo = 60
        , spOrdersHi = 140
        , spClampRest = clamp
        }
  (gs, _) <- runSimH defaultHooks sp (initState sp)
  let done = 7
  pure $ case parseState (stateText sp done gs) of
    Left err -> counterexample err False
    Right (sp', done', gs') -> conjoin
      [ counterexample "params" (show sp == show sp')
      , counterexample "done" (done == done')
      , counterexample "rng" (show (gsRng gs) == show (gsRng gs'))
      , counterexample "book" (show (gsBook gs) == show (gsBook gs'))
      , counterexample "anchor" (gsAnchor gs == gsAnchor gs')
      , counterexample "candle" (gsCandle gs == gsCandle gs')
      ]

-- resuming a tick-mode run: the appended segment's rows are the tail of
-- a single combined run with candle indices shifted past the candles
-- already written
prop_resume_ticks :: Word64 -> SimMode -> Property
prop_resume_ticks seed mode = ioProperty $ do
  let sp = defaultParams
        { spSeed = seed
        , spCandles = 40
        , spOrdersLo = 50
        , spOrdersHi = 50
        , spMode = mode
        , spBookDepth = 5
        }
      (_, tsAll, _psAll, _evAll) = runSimTicks sp
      rows = map (tickRow 100000 5)
      lb0 = trFromBook 0 0 (gsBook (initState sp))
  ref <- newIORef []
  (gs1, cs1, _n1) <- runSimTicksH defaultHooks (collect ref) ignoreLvl sp { spCandles = 20 } 0 (initState sp) lb0
  ts1 <- reverse <$> readIORef ref
  writeIORef ref []
  (_gs2, _cs2, _n2) <- runSimTicksH defaultHooks (collect ref) ignoreLvl sp { spCandles = 20 } (length cs1) gs1 (trFromBook 0 (length cs1) (gsBook gs1))
  ts2 <- reverse <$> readIORef ref
  pure (rows (ts1 ++ ts2) == rows tsAll)
  where
    collect ref t = modifyIORef' ref (t :)
    ignoreLvl _ = pure ()

-- the walk-forward search must find near-perfect parameters for an exact
-- harmonic (its forecast is essentially exact, so the hybrid score ~ 0)
prop_fft_best_finds_sinusoid :: Bool
prop_fft_best_finds_sinusoid =
  let n = 1500
      f = 8 / 512 :: Double
      sig t = 100 + 5 * cos (2 * pi * f * fromIntegral t)
      closes = [sig t | t <- [0 .. n - 1]]
      (w, k, sc) = fftBest 50 32 closes
  in w > 0 && k > 0 && sc < 0.1

-- a flat candidate set must yield the no-result sentinel (series too
-- short for any anchor with the mandatory 50-candle overlap)
prop_fft_best_no_data :: Bool
prop_fft_best_no_data =
  fftBest 50 64 [1 .. 40] == (0, 0, 0)

-- deterministic pseudo-noise (xorshift64*) shared by the entropy props
lcgNoise :: [Double]
lcgNoise = go (42 :: Word64)
  where
    go w = val : go w3
      where
        w1 = w `xor` (w `shiftL` 13)
        w2 = w1 `xor` (w1 `shiftR` 7)
        w3 = w2 `xor` (w2 `shiftL` 17)
        val = fromIntegral ((w3 * 2685821657736338717) `shiftR` 11) / 9007199254740992 - 0.5

-- a monotone series has a single ordinal pattern
prop_ks_monotone_zero :: Bool
prop_ks_monotone_zero = permEntropy 5 [1 .. 400] <= 1e-12

-- a strict 2-cycle at m=4 has exactly two equally frequent ordinal
-- patterns, so H = ln 2
prop_ks_alternating_two_patterns :: Bool
prop_ks_alternating_two_patterns =
  abs (permEntropy 4 (take 400 (cycle [0, 1])) - log 2 / log 24) < 1e-3

-- iid noise saturates ln(m!): finite-sample bias at n=2000, m=5 is
-- ~0.03 in absolute H, so normalized stays well above 0.95
prop_ks_iid_near_max :: Bool
prop_ks_iid_near_max = permEntropy 5 (take 2000 lcgNoise) > 0.95

-- rolling output: one entry per candle, NaN warm-up, and the last value
-- matches the plain whole-window entropy
prop_ks_rolling_matches_window :: Bool
prop_ks_rolling_matches_window =
  let xs = take 300 lcgNoise
      win = 100
      m = 4
      r = rollingPermEntropy win m xs
      ref = permEntropy m (drop (length xs - win) xs)
  in length r == 300
     && all isNaN (take (win - 1) r)
     && not (isNaN (last r))
     && abs (last r - ref) < 1e-12

-- rolling over a short window tracks each window's entropy, not just the
-- last one
prop_ks_rolling_middle_matches :: Bool
prop_ks_rolling_middle_matches =
  let xs = take 260 lcgNoise
      win = 60
      m = 3
      r = rollingPermEntropy win m xs
      ok i = abs (r !! (i - 1) - permEntropy m (take win (drop (i - win) xs))) < 1e-12
  in all ok [win, win + 40, win + 80, length xs]

prop_ks_rolling_invalid :: Bool
prop_ks_rolling_invalid =
  let xs = take 100 lcgNoise
      short = rollingPermEntropy 200 4 xs
      badWin = rollingPermEntropy 2 5 xs
      ys = take 60 lcgNoise
      badM = rollingPermEntropy 50 99 ys
      ref = permEntropy 7 (drop (60 - 50) ys)
  in all isNaN short
      && all isNaN badWin
      && not (isNaN (last badM))
      && abs (last badM - ref) < 1e-12

-- closed-form JSD(P, U) used by Sim.Entropy (counts over k bins, T the
-- window's pattern count) against the textbook H(M) - (H(P) + H(U)) / 2
cecpJsd :: [Int] -> Double
cecpJsd counts = log t / 2 + s / (2 * t) - hsum / t - log k / 2
  where
    k = fromIntegral (length counts)
    t = fromIntegral (sum counts)
    s = sum [fromIntegral c * log (fromIntegral c) | c <- counts, c > 0]
    hsum = sum [m * log m | c <- counts, let m = (fromIntegral c + t / k) / 2]

cecpJsdMax :: Int -> Double
cecpJsdMax k = log 2 + log k' / 2 - (k' + 1) / (2 * k') * log (k' + 1)
  where
    k' = fromIntegral k

jsdBrute :: [Double] -> Double
jsdBrute ps = h ms - (h ps + log k) / 2
  where
    k = fromIntegral (length ps)
    h xs = negate (sum [if x <= 0 then 0 else x * log x | x <- xs])
    ms = map (\p -> (p + 1 / k) / 2) ps

genCounts :: Gen [Int]
genCounts = do
  k <- chooseInt (2, 24)
  cs <- vectorOf k (chooseInt (0, 100))
  pure (if sum cs == 0 then 1 : cs else cs)

-- the closed-form JSD must equal the brute-force definition on any
-- histogram (including empty bins, where M still carries U's mass)
prop_cecp_jsd_matches_bruteforce :: Property
prop_cecp_jsd_matches_bruteforce = forAll genCounts $ \cs ->
  let t = sum cs
      ps = map (\c -> fromIntegral c / fromIntegral t) cs
  in abs (cecpJsd cs - jsdBrute ps) < 1e-10

-- the MPR normalizer JSD(delta, U) is attained by a one-hot histogram
prop_cecp_jsdmax_is_delta :: Property
prop_cecp_jsdmax_is_delta = forAll (chooseInt (2, 24)) $ \k ->
  let cs = 37 : replicate (k - 1) 0
  in abs (cecpJsd cs - cecpJsdMax k) < 1e-10

-- a monotone series has a single ordinal pattern: H = 0 and C = 0
prop_cecp_monotone_zero :: Bool
prop_cecp_monotone_zero =
  let (h, c) = cecpOf 5 [1 .. 400]
  in h <= 1e-12 && c <= 1e-12

-- iid noise: H near 1, C near 0 (pattern histogram near-uniform)
prop_cecp_iid_noise_corner :: Bool
prop_cecp_iid_noise_corner =
  let (h, c) = cecpOf 5 (take 4000 lcgNoise)
  in h > 0.95 && c < 0.05

-- rolling CECP: one (H, C) pair per candle, NaN warm-up on both, last
-- pair matches the direct window computation
prop_cecp_rolling_matches_window :: Bool
prop_cecp_rolling_matches_window =
  let xs = take 300 lcgNoise
      win = 100
      m = 4
      r = rollingCECP win m xs
      (refH, refC) = cecpOf m (drop (length xs - win) xs)
      ok (h, c) = not (isNaN h) && not (isNaN c) && h >= -1e-12 && h <= 1 + 1e-12 && c >= -1e-12 && c <= h + 1e-9
  in length r == 300
     && all (\(h, c) -> isNaN h && isNaN c) (take (win - 1) r)
     && all ok (drop (win - 1) r)
     && abs (fst (last r) - refH) < 1e-12
     && abs (snd (last r) - refC) < 1e-12

prop_cecp_rolling_invalid :: Bool
prop_cecp_rolling_invalid =
  let xs = take 100 lcgNoise
      short = rollingCECP 200 4 xs
      badWin = rollingCECP 2 5 xs
  in length short == 100
     && all (\(h, c) -> isNaN h && isNaN c) short
     && all (\(h, c) -> isNaN h && isNaN c) badWin

-- endpoint weighting must pin the fit to the first candle: with a spike
-- at t=0 plain LS absorbs only a fraction of it, weighted LS (edgeW
-- large) absorbs nearly all
prop_fft_edge_weight :: Bool
prop_fft_edge_weight =
  let n = 256
      series = [ (if t == 0 then 120 else 100) + 3 * cos (2 * pi * fromIntegral t / 64) | t <- [0 .. n - 1] ]
      errOf fit = abs (head fit - head series)
      (fitP, _) = fftApprox 8 1 series 0 n 16
      (fitW, _) = fftApprox 8 1000 series 0 n 16
      eP = errOf fitP
      eW = errOf fitW
  in eW < eP - 3

-- --- ML trainer props -----------------------------------------------------

-- synthetic bars: smooth sinus with a small wick
mlSinusBars :: Int -> [MLD.Bar]
mlSinusBars n =
  [ let p = 100 + 3 * sin (2 * pi * fromIntegral i / 50)
    in MLD.Bar (p - 0.05) (p + 0.05) (p - 0.05) (p + 0.05) 1000
  | i <- [0 .. n - 1] ]

prop_ml_parse_bars_sim_csv :: Word64 -> Property
prop_ml_parse_bars_sim_csv seed =
  let p = defaultParams { spSeed = seed, spCandles = 30, spOrdersLo = 50, spOrdersHi = 50 }
      cs = runSim p
      scale = fromIntegral (spPriceScale p)
      toBar c = MLD.Bar
        (fromIntegral (unPrice (candleOpen c)) / scale)
        (fromIntegral (unPrice (candleHigh c)) / scale)
        (fromIntegral (unPrice (candleLow c)) / scale)
        (fromIntegral (unPrice (candleClose c)) / scale)
        (fromIntegral (candleVol c))
      expect = map toBar cs
  in case MLD.parseBars (csvText p cs) of
       Left err -> property (counterexample err False)
       Right got ->
         property (length got == length expect)
         .&&. property (all closeBar (zip got expect))
  where
    closeBar (a, b) =
      abs (MLD.barO a - MLD.barO b) < 1e-9 && abs (MLD.barH a - MLD.barH b) < 1e-9
      && abs (MLD.barL a - MLD.barL b) < 1e-9 && abs (MLD.barC a - MLD.barC b) < 1e-9
      && MLD.barV a == MLD.barV b

prop_ml_parse_bars_timestamped :: Bool
prop_ml_parse_bars_timestamped =
  let csv = "YYYY.MM.DD HH:MM,o,h,l,c,vol\n"
         ++ "2024.01.02 03:04,1.1,1.2,1.0,1.15,100\n"
         ++ "2024.01.02 03:05,1.15,1.3,1.1,1.25,200\n"
      want = [MLD.Bar 1.1 1.2 1.0 1.15 100, MLD.Bar 1.15 1.3 1.1 1.25 200]
  in MLD.parseBars csv == Right want

prop_ml_parse_bars_garbage :: Bool
prop_ml_parse_bars_garbage =
  MLD.parseBars "open,high,low,close,vol\n" == Left "no parseable candle rows"

prop_ml_features_rows_valid :: Bool
prop_ml_features_rows_valid =
  let spec = MLD.defaultFeatureSpec
      fs = MLD.buildFeatures spec (mlSinusBars 800)
      size = MLD.featureSize spec
      ok i = let r = MLD.fRows fs V.! i
             in VU.length r == size && VU.all (\x -> not (isNaN x) && not (isInfinite x)) r
  in MLD.fWarmup fs == 20
     && all (\i -> VU.null (MLD.fRows fs V.! i)) [0 .. MLD.fWarmup fs - 1]
     && all ok [MLD.fWarmup fs .. 799 - 2]
     && MLD.fLabels fs VU.! 799 == -1
     && VU.all (\l -> l >= 0 && l <= 2) (VU.slice (MLD.fWarmup fs) (800 - MLD.fWarmup fs - 1) (MLD.fLabels fs))

-- the optional entropy feature extends the row by one in [0,1] and
-- pushes the warm-up out to the entropy window
prop_ml_features_ks :: Bool
prop_ml_features_ks =
  let spec = MLD.defaultFeatureSpec { MLD.fsKsWin = 30, MLD.fsKsM = 4 }
      fs = MLD.buildFeatures spec (mlSinusBars 200)
      size = MLD.featureSize spec
      ks i = (MLD.fRows fs V.! i) VU.! (size - 1)
  in size == MLD.featureSize MLD.defaultFeatureSpec + 1
     && MLD.fWarmup fs == 30
     && all (\i -> let x = ks i in not (isNaN x) && x >= 0 && x <= 1) [30 .. 198]

prop_ml_prepare_splits_and_norm :: Bool
prop_ml_prepare_splits_and_norm =
  let tp = MLT.defaultTrainParams
  in case MLT.prepareData tp (mlSinusBars 800) of
       Left err -> error err
       Right md ->
         let size = MLD.featureSize (MLT.tpSpec tp)
             tr = MLT.mdTrain md
             va = MLT.mdVal md
             te = MLT.mdTest md
             mean k idxs = sum [ (MLT.mdRows md V.! i) VU.! k | i <- idxs ] / fromIntegral (length idxs)
             -- no overlap, ordered, non-empty
             disjoint = all (`notElem` tr) (va ++ te) && all (`notElem` va) te
         in length tr > 0 && length va > 0 && length te > 0
            && disjoint
            && all (> head tr) va && all (> last va) te
            && all (\i -> VU.length (MLT.mdRows md V.! i) == size) (tr ++ va ++ te)
            && all (\k -> abs (mean k tr) < 1e-9) [0 .. size - 1]

prop_ml_softmax_sums_to_one :: Bool
prop_ml_softmax_sums_to_one =
  let m = MLM.initModel (mkSMGen 5) [7, 16, 3]
      fs = MLD.buildFeatures MLD.defaultFeatureSpec (mlSinusBars 100)
      x = MLD.fRows fs V.! 50
      ps = MLM.predict m x
  in abs (VU.sum ps - 1) < 1e-12 && MLM.predictClass m x >= 0 && MLM.predictClass m x <= 2

prop_ml_init_deterministic :: Bool
prop_ml_init_deterministic =
  let a = MLM.initModel (mkSMGen 9) [4, 8, 3]
      b = MLM.initModel (mkSMGen 9) [4, 8, 3]
      c = MLM.initModel (mkSMGen 10) [4, 8, 3]
  in a == b && a /= c && MLM.layerSizes a == [4, 8, 3]

-- analytic gradient vs central finite differences on a tiny model
prop_ml_grad_matches_numeric :: Bool
prop_ml_grad_matches_numeric =
  let m0 = MLM.initModel (mkSMGen 11) [2, 3, 2]
      xs = [VU.fromList [0.3, -0.7], VU.fromList [1.1, 0.2], VU.fromList [-0.4, 0.9]]
      ys = [1, 0, 1]
      batch = zip xs ys
      grads = zipWith (\x y -> MLM.gradOne m0 x y) xs ys
      combined = foldr1 (\a b -> zipWith addG a b) grads
      num l i =
        let d = 1e-5
            lp = MLM.loss (perturb m0 l i d) batch
            lm = MLM.loss (perturb m0 l i (-d)) batch
        in (lp - lm) / (2 * d)
      check l g =
        VU.and (VU.imap (\i gi -> abs (gi / n - num l i) < 1e-4) (MLM.lgW g))
      n = fromIntegral (length batch)
      addG a b = MLM.LayerGrad (VU.zipWith (+) (MLM.lgW a) (MLM.lgW b)) (VU.zipWith (+) (MLM.lgB a) (MLM.lgB b))
  in all (\l -> check l (combined !! l)) [0, 1]
  where
    perturb m l i d =
      let layers = MLM.mLayers m
          lay = layers !! l
          w' = MLM.lW lay VU.// [(i, MLM.lW lay VU.! i + d)]
          lay' = lay { MLM.lW = w' }
      in m { MLM.mLayers = take l layers ++ [lay'] ++ drop (l + 1) layers }

prop_ml_sgd_decreases_loss :: Bool
prop_ml_sgd_decreases_loss =
  let samples = [ (VU.fromList [a, b], if a > b then 2 else 0)
                | (a, b) <- [(0.9, 0.1), (0.8, -0.2), (-0.3, 0.6), (0.1, 0.7), (0.7, 0.4), (-0.6, -0.9)] ]
      m0 = MLM.initModel (mkSMGen 7) [2, 4, 3]
      l0 = MLM.loss m0 samples
      m1 = MLM.sgdStep 0.1 m0 samples
      l1 = MLM.loss m1 samples
      mN = iterate (\m -> MLM.sgdStep 0.5 m samples) m1 !! 300
      acc = sum [ if MLM.predictClass mN x == y then 1 else 0 | (x, y) <- samples ]
  in l1 < l0 && acc >= 5

-- an always-long policy on monotone rising bars: one trade in, positive
-- PnL, zero drawdown, Calmar falls back to PnL
prop_ml_policy_always_long :: Bool
prop_ml_policy_always_long =
  let bars = [ let c = 100 * 1.001 ^ i
               in MLD.Bar c c c c 1000
             | i <- [0 .. 299] :: [Int] ]
      tp = MLT.defaultTrainParams { MLT.tpFee = 0.0002 }
      Right md = MLT.prepareData tp bars
      fi = MLD.featureSize (MLT.tpSpec tp)
      long = MLM.Model [ MLM.Layer fi 3 (VU.replicate (fi * 3) 0) (VU.fromList [0, 0, 5]) ]
      (met, eq) = MLT.evalPolicy long (MLT.tpFee tp) md (MLT.mdTest md)
  in MLT.mPnl met > 0
     && MLT.mMaxDD met == 0
     && MLT.mCalmar met == MLT.mPnl met
     && MLT.mTrades met == 1
     && VU.length eq == length (MLT.mdTest md)

-- end-to-end session on predictable (sinus) bars must beat chance by a
-- wide margin; fixed seed, deterministic
prop_ml_session_learns_sinus :: Bool
prop_ml_session_learns_sinus =
  let tp = MLT.defaultTrainParams { MLT.tpEpochs = 40, MLT.tpLR = 0.1, MLT.tpHidden = [8] }
      Right md = MLT.prepareData tp (mlSinusBars 800)
      res = unsafePerformIO (MLT.trainSession MLT.defaultTrainHooks tp md 3)
  in case res of
       Nothing -> False
       Just sr ->
         let m = MLT.srMetrics sr
         in MLT.srBestEpoch sr > 0
            && MLT.mAccuracy m > 0.6
            && MLT.mAccuracy m <= 1
            && MLT.mPnl m /= 0
            && VU.length (MLT.srEquity sr) == length (MLT.mdTest md)

main :: IO ()
main = do
  results <- mapM runProp props
  if all (isSuccess . snd) results then exitSuccess else exitFailure

runProp :: (String, Property) -> IO (String, Result)
runProp (name, prop) = do
  putStrLn ("\n== " ++ name)
  r <- quickCheckWithResult (stdArgs { maxSuccess = 500 }) prop
  pure (name, r)

props :: [(String, Property)]
props =
  [ ("no_crossed_book", property prop_no_crossed_book)
  , ("positive_volumes", property prop_positive_volumes)
  , ("trades_at_ask_prices", property prop_trades_at_ask_prices)
  , ("trade_volume_conserved", property prop_trade_volume_conserved)
  , ("candle_invariants", property prop_candle_invariants)
  , ("sim_produces_valid_candles", property prop_sim_produces_valid_candles)
  , ("sim_valid_with_options", property prop_sim_valid_with_options)
  , ("sim_valid_with_order_range", property prop_sim_valid_with_order_range)
  , ("sim_no_dead_market", property prop_sim_no_dead_market)
  , ("sim_percent_offsets_positive", property prop_sim_percent_offsets_positive)
  , ("sim_zero_floor", property prop_sim_zero_floor)
  , ("ticks_candles_match_runSim_percent", property prop_ticks_candles_match_runSim_percent)
  , ("ticks_candles_match_runSim", property prop_ticks_candles_match_runSim)
  , ("ticks_book_snapshots", property prop_ticks_book_snapshots)
  , ("lvl_tracker_matches_model", property prop_lvl_tracker_matches_model)
  , ("lvl_segments_coherent", property prop_lvl_segments_coherent)
  , ("lvl_events_flat_consistent", property prop_lvl_events_flat_consistent)
  , ("resume_candles", property prop_resume_candles)
  , ("state_roundtrip", property prop_state_roundtrip)
  , ("sinus_candles_valid", property prop_sinus_candles_valid)
  , ("sinus_periodicity", property prop_sinus_periodicity)
  , ("sinus_matches_formula", property prop_sinus_matches_formula)
  , ("sinus_volumes_periodic", property prop_sinus_volumes_periodic)
  , ("sinus_segments_join", property prop_sinus_segments_join)
  , ("sinus_state_roundtrip", property prop_sinus_state_roundtrip)
  , ("resume_ticks", property prop_resume_ticks)
  , ("fft_recovers_sinusoid", property prop_fft_recovers_sinusoid)
  , ("fft_recovers_sinusoid_offset", property prop_fft_recovers_sinusoid_offset)
  , ("fft_recovers_sinusoid_fractional", property prop_fft_recovers_sinusoid_fractional)
  , ("fft_recovers_multiwave_fractional", property prop_fft_recovers_multiwave_fractional)
  , ("fft_refine_local", property prop_fft_refine_local)
  , ("fft_fit_covers_window", property prop_fft_fit_covers_window)
  , ("fft_waves_limit", property prop_fft_waves_limit)
  , ("fft_gram_matches_bruteforce", property prop_fft_gram_matches_bruteforce)
  , ("fft_solve_residual", property prop_fft_solve_residual)
  , ("fft_forecast_bounded", property prop_fft_forecast_bounded)
  , ("fft_forecast_length", property prop_fft_forecast_length)
  , ("fft_forecast_invalid", property prop_fft_forecast_invalid)
  , ("fft_best_finds_sinusoid", property prop_fft_best_finds_sinusoid)
  , ("fft_best_no_data", property prop_fft_best_no_data)
  , ("fft_edge_weight", property prop_fft_edge_weight)
  , ("ks_monotone_zero", property prop_ks_monotone_zero)
  , ("ks_alternating_two_patterns", property prop_ks_alternating_two_patterns)
  , ("ks_iid_near_max", property prop_ks_iid_near_max)
  , ("ks_rolling_matches_window", property prop_ks_rolling_matches_window)
  , ("ks_rolling_middle_matches", property prop_ks_rolling_middle_matches)
  , ("ks_rolling_invalid", property prop_ks_rolling_invalid)
  , ("cecp_jsd_matches_bruteforce", prop_cecp_jsd_matches_bruteforce)
  , ("cecp_jsdmax_is_delta", prop_cecp_jsdmax_is_delta)
  , ("cecp_monotone_zero", property prop_cecp_monotone_zero)
  , ("cecp_iid_noise_corner", property prop_cecp_iid_noise_corner)
  , ("cecp_rolling_matches_window", property prop_cecp_rolling_matches_window)
  , ("cecp_rolling_invalid", property prop_cecp_rolling_invalid)
  , ("ml_parse_bars_sim_csv", property prop_ml_parse_bars_sim_csv)
  , ("ml_parse_bars_timestamped", property prop_ml_parse_bars_timestamped)
  , ("ml_parse_bars_garbage", property prop_ml_parse_bars_garbage)
  , ("ml_features_rows_valid", property prop_ml_features_rows_valid)
  , ("ml_features_ks", property prop_ml_features_ks)
  , ("ml_prepare_splits_and_norm", property prop_ml_prepare_splits_and_norm)
  , ("ml_softmax_sums_to_one", property prop_ml_softmax_sums_to_one)
  , ("ml_init_deterministic", property prop_ml_init_deterministic)
  , ("ml_grad_matches_numeric", property prop_ml_grad_matches_numeric)
  , ("ml_sgd_decreases_loss", property prop_ml_sgd_decreases_loss)
  , ("ml_policy_always_long", property prop_ml_policy_always_long)
  , ("ml_session_learns_sinus", property prop_ml_session_learns_sinus)
  ]
