module Main (main) where

import Data.List (sort)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Sim.Engine
import Sim.Fft (corr, fftApprox, fftBest, fftForecast, gram, solveLS)
import Sim.Gen
import Sim.TickGen (runSimTicks)
import Sim.Types
import Test.QuickCheck

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
      (cs, _ts) = runSimTicks sp
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
      (cs, _ts) = runSimTicks sp
  in property $ cs == runSim sp

prop_ticks_book_snapshots :: Word64 -> SimMode -> Property
prop_ticks_book_snapshots seed mode =
  let sp = defaultParams { spSeed = seed, spCandles = 20, spOrdersLo = 50, spOrdersHi = 50, spMode = mode, spBookDepth = 10 }
      (cs, ts) = runSimTicks sp
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
      (_, fc) = fftApprox 0 series 0 n 32
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
      (_, fc) = fftApprox 0 series off 356 16
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
      ys = map (fromIntegral . abs) seeds
      bruteC = concat [ [sum (zipWith (*) (map (cos . (wj *)) ts) ys), sum (zipWith (*) (map (sin . (wj *)) ts) ys)] | wj <- ws ]
      closedC = concatMap (\w -> let (c, s) = corr n ys w in [c, s]) ws
      close a b = abs (a - b) < 1e-6
  in length closed == length brute && and (zipWith close (concat closed) (concat brute))
     && and (zipWith close closedC bruteC)

-- on a well-conditioned system the solve must satisfy the normal
-- equations (validates the Jacobi eigendecomposition end to end)
prop_fft_solve_residual :: [Int] -> Bool
prop_fft_solve_residual seeds =
  let n = 200
      freqs = [1 / 8, 1 / 20, 3 / 16 :: Double]
      ws = map (2 * pi *) freqs
      ys = take n (map (fromIntegral . abs) seeds ++ repeat 0.5)
      g = gram n ws
      b = concatMap (\w -> let (c, s) = corr n ys w in [c, s]) ws
      x = solveLS n ys freqs
      resid = sqrt (sum [ (sum (zipWith (*) (g !! i) x) - b !! i) ^ 2 | i <- [0 .. length b - 1] ])
      bnorm = sqrt (sum [ bi ^ 2 | bi <- b ])
  in bnorm > 0 && resid / bnorm < 1e-6

-- extrapolation stays bounded even at the max wave count: the truncated
-- eigensolve cannot produce the runaway coefficients that shoot the
-- forecast out of the plot
prop_fft_forecast_bounded :: [Int] -> Property
prop_fft_forecast_bounded seeds =
  let n = 64
      ys = [ fromIntegral (abs s `mod` 100) | s <- take n seeds ]
      (_, fc) = fftApprox (n `div` 4) ys 0 n 16
      ymax = maximum (map abs ys) + 1
  in not (null fc) ==> maximum (map abs fc) < 10 * ymax

-- the in-window fit must reproduce the window itself
prop_fft_fit_covers_window :: Bool
prop_fft_fit_covers_window =
  let n = 256
      f = 1 / 16 :: Double
      sig t = 100 + 3 * cos (2 * pi * f * fromIntegral t)
      series = [sig t | t <- [0 .. n - 1]]
      (fit, _) = fftApprox 0 series 0 n 32
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
      (fit, fc) = fftApprox 1 series 0 n 16
      bad = maximum
        (map abs (zipWith (-) fit (map main [0 .. n - 1]))
           ++ map abs (zipWith (-) fc (map main [n .. n + 15])))
  in bad < 1e-6

-- length contract: full window -> (window, predict) points, short window
-- or non-positive predict -> nothing
prop_fft_forecast_length :: Int -> Int -> Int -> Property
prop_fft_forecast_length total w p =
  let total' = 16 + abs total `mod` 500
      w' = abs w `mod` (total' + 1)
      p' = abs p `mod` 100 + 1
      series = map (sin . fromIntegral) [0 .. total' - 1]
      (fit, fc) = fftApprox 0 series 0 w' p'
  in (w' >= 16) ==> length fit == w' && length fc == p'

prop_fft_forecast_invalid :: [Int] -> Bool
prop_fft_forecast_invalid xs =
  let series = map fromIntegral xs
      (f1, c1) = fftApprox 0 series 0 100 0
      (f2, c2) = fftApprox 0 series 0 5 10
      (f3, c3) = fftApprox 0 series 0 100 (-3)
  in null f1 && null c1 && null f2 && null c2 && null f3 && null c3

-- the walk-forward search must find near-perfect parameters for an exact
-- harmonic (its forecast is essentially exact, so the hybrid score ~ 0)
prop_fft_best_finds_sinusoid :: Bool
prop_fft_best_finds_sinusoid =
  let n = 3000
      f = 8 / 512 :: Double
      sig t = 100 + 5 * cos (2 * pi * f * fromIntegral t)
      closes = [sig t | t <- [0 .. n - 1]]
      (w, k, sc) = fftBest 50 64 closes
  in w > 0 && k > 0 && sc < 0.1

-- a flat candidate set must yield the no-result sentinel (series too
-- short for any anchor with the mandatory 50-candle overlap)
prop_fft_best_no_data :: Bool
prop_fft_best_no_data =
  fftBest 50 64 [1 .. 40] == (0, 0, 0)

main :: IO ()
main = mapM_ (quickCheckWith (stdArgs { maxSuccess = 500 }))
  [ property prop_no_crossed_book
  , property prop_positive_volumes
  , property prop_trades_at_ask_prices
  , property prop_trade_volume_conserved
  , property prop_candle_invariants
  , property prop_sim_produces_valid_candles
  , property prop_sim_valid_with_options
  , property prop_sim_valid_with_order_range
  , property prop_sim_no_dead_market
  , property prop_sim_percent_offsets_positive
  , property prop_sim_zero_floor
  , property prop_ticks_candles_match_runSim_percent
  , property prop_ticks_candles_match_runSim
  , property prop_ticks_book_snapshots
  , property prop_fft_recovers_sinusoid
  , property prop_fft_recovers_sinusoid_offset
  , property prop_fft_fit_covers_window
  , property prop_fft_waves_limit
  , property prop_fft_gram_matches_bruteforce
  , property prop_fft_solve_residual
  , property prop_fft_forecast_bounded
  , property prop_fft_forecast_length
  , property prop_fft_forecast_invalid
  , property prop_fft_best_finds_sinusoid
  , property prop_fft_best_no_data
  ]
