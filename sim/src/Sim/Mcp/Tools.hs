{-# LANGUAGE OverloadedStrings #-}

-- MCP tool definitions for the rel simulator: sim generation/resume,
-- indicators, strategy backtest/optimize, ML training, and data access.
-- Tools return a JSON payload (wrapped as text content by the server);
-- runs whose output exceeds `inline_limit` return a file path plus a
-- summary instead of full arrays.
module Sim.Mcp.Tools (tools, paramsFromJson) where

import Control.Exception (SomeException, catch, fromException, try)
import Control.Monad (when)
import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Sim.Cli (applyArgs)
import Sim.Csv (levelsPath, parseTradesBin, ticksPath)
import Sim.Entropy (rollingCECP, rollingPermEntropy)
import Sim.Fft (fftApprox)
import Sim.Gen (GenState (..))
import Sim.Markov (MarkovParams (..), markovSeriesStates, statesOf)
import qualified Sim.ML.Data as MLD
import qualified Sim.ML.Train as MLT
import Sim.Mcp.Editor (editorTools)
import Sim.Mcp.Server (Tool (..), ToolCtx (..), toolErrorResult, toolTextResult)
import qualified Sim.Run as Run
import Sim.Resume (readState, statePath, writeState)
import qualified Sim.Strat.Backtest as STB
import qualified Sim.Strat.Dsl as STD
import qualified Sim.Strat.Optimize as STO
import Sim.Types
import System.Directory (doesFileExist, getTemporaryDirectory, listDirectory)
import System.IO.Error (isDoesNotExistError, userError)

tools :: [Tool]
tools =
  [ toolSimGenerate
  , toolSimResume
  , toolPermEntropy
  , toolCecp
  , toolFft
  , toolMarkov
  , toolObvol
  , toolDslBacktest
  , toolStratOptimize
  , toolTrades
  , toolMlTrain
  , toolReadSeries
  , toolListFiles
  ] ++ editorTools

-- --- argument helpers -------------------------------------------------------

type Args = KM.KeyMap Value

asArgs :: Value -> Either String Args
asArgs (Object o) = Right o
asArgs _          = Left "arguments must be a JSON object"

req :: Args -> Text -> Either String Value
req o k = maybe (Left ("missing argument: " ++ T.unpack k)) Right (KM.lookup (AK.fromText k) o)

reqStr :: Args -> Text -> Either String String
reqStr o k = do
  v <- req o k
  case v of
    String s -> Right (T.unpack s)
    _        -> Left (T.unpack k ++ " must be a string")

reqInt :: Args -> Text -> Either String Int
reqInt o k = do
  v <- req o k
  case v of
    Number n -> Right (floor n)
    _        -> Left (T.unpack k ++ " must be a number")

reqDouble :: Args -> Text -> Either String Double
reqDouble o k = do
  v <- req o k
  case v of
    Number n -> Right (realToFrac n)
    _        -> Left (T.unpack k ++ " must be a number")

optInt :: Args -> Text -> Int -> Either String Int
optInt o k d = maybe (Right d) (\v -> case v of
  Number n -> Right (floor n)
  _        -> Left (T.unpack k ++ " must be a number")) (KM.lookup (AK.fromText k) o)

optDouble :: Args -> Text -> Double -> Either String Double
optDouble o k d = maybe (Right d) (\v -> case v of
  Number n -> Right (realToFrac n)
  _        -> Left (T.unpack k ++ " must be a number")) (KM.lookup (AK.fromText k) o)

optBool :: Args -> Text -> Bool -> Either String Bool
optBool o k d = maybe (Right d) (\v -> case v of
  Bool b -> Right b
  _      -> Left (T.unpack k ++ " must be a boolean")) (KM.lookup (AK.fromText k) o)

-- IO variants for tool handlers (bad arguments become userErrors caught
-- by the server and reported as tool errors)
asArgsIO :: Value -> IO Args
asArgsIO = either (ioError . userError) pure . asArgs

reqIO :: Args -> Text -> IO Value
reqIO o = either (ioError . userError) pure . req o

reqStrIO :: Args -> Text -> IO String
reqStrIO o = either (ioError . userError) pure . reqStr o

reqIntIO :: Args -> Text -> IO Int
reqIntIO o = either (ioError . userError) pure . reqInt o

reqDoubleIO :: Args -> Text -> IO Double
reqDoubleIO o = either (ioError . userError) pure . reqDouble o

optIntIO :: Args -> Text -> Int -> IO Int
optIntIO o k d = either (ioError . userError) pure (optInt o k d)

optDoubleIO :: Args -> Text -> Double -> IO Double
optDoubleIO o k d = either (ioError . userError) pure (optDouble o k d)

optBoolIO :: Args -> Text -> Bool -> IO Bool
optBoolIO o k d = either (ioError . userError) pure (optBool o k d)

-- sim params as a JSON object with CLI keys (no "--"): numbers and
-- booleans become their CLI scalar text; reuses Sim.Cli.applyArgs so the
-- MCP surface accepts exactly the CLI keys
paramsFromJson :: Value -> Either String SimParams
paramsFromJson (Object o) = applyArgs (concatMap toArg (KM.toList o))
  where
    toArg (k, v) = ["--" ++ map dash (T.unpack (AK.toText k)), scalarText v]
    dash '_' = '-'
    dash c   = c
    scalarText (String s) = T.unpack s
    scalarText (Bool b)   = if b then "on" else "off"
    scalarText (Number n) = case floatingOrInteger n of
      Right i -> show (i :: Integer)
      Left d  -> show (d :: Double)
    scalarText _          = ""
paramsFromJson _ = Left "params must be a JSON object"

-- closes either inline ("closes": [..]) or from a candles csv/rbin
-- ("path"); the optional "limit" truncates file-loaded series (0 = all)
getCloses :: Args -> IO (Either String [Double])
getCloses o = case KM.lookup "closes" o of
  Just (Array a) -> pure (Right (map toD (V.toList a)))
  Just _         -> pure (Left "closes must be an array of numbers")
  Nothing        -> case KM.lookup "path" o of
    Just (String p) -> do
      parsed <- MLD.loadBars (T.unpack p)
      pure (case parsed of
        Left err -> Left err
        Right bars ->
          let cs = map MLD.barC bars
          in Right (if limit == 0 then cs else take limit cs))
    Just _  -> pure (Left "path must be a string")
    Nothing -> pure (Left "provide either closes or path")
  where
    toD (Number n) = realToFrac n
    toD _          = 0 / 0
    limit = either (const 0) id (optInt o "limit" 0)

catching :: IO (Either String Value) -> IO Value
catching act = do
  r <- try act :: IO (Either SomeException (Either String Value))
  pure (case r of
    Left err -> toolErrorResult (show err)
    Right v  -> either toolErrorResult toolTextResult v)

schema :: [(Text, Text, Value)] -> Value
schema required = object
  [ "type" .= ("object" :: Text)
  , "properties" .= object []
  , "required" .= map (\(k, _, _) -> k) required
  ]

-- --- sim.generate -----------------------------------------------------------

toolSimGenerate :: Tool
toolSimGenerate = Tool
  { toolName = "sim.generate"
  , toolDescription = "Run the order-book candle generator (or sinus mode) and write the candles csv + continuation state. Tick modes also stream ticks/levels/vev sidecars. Runs at or below inline_limit additionally return the candles inline."
  , toolSchema = schema
      [ ("params", "sim parameters object (CLI keys without --)", Object KM.empty) ]
  , toolRun = \ctx args -> catching $ do
      o <- asArgsIO args
      pv <- reqIO o "params"
      p <- either (ioError . userError) pure (paramsFromJson pv)
      inlineLimit <- optIntIO o "inline_limit" 10000
      outPath <- resolveOut o "rel-mcp-gen"
      let hooks = simHooksFor ctx (spCandles p)
      res <- try (Run.generateCsv hooks outPath p) :: IO (Either SomeException ())
      case res of
        Left err
          | Just SimCancelled <- fromException err -> do
              Run.cleanupOutput outPath p
              pure (Right (object ["cancelled" .= True, "path" .= outPath]))
          | otherwise -> pure (Left (show err))
        Right _ -> do
          parsed <- MLD.loadBars outPath
          pure (case parsed of
            Left err -> Left err
            Right bars -> Right (seriesResult inlineLimit outPath (map MLD.barO bars) (map MLD.barH bars)
                                   (map MLD.barL bars) (map MLD.barC bars) (map MLD.barV bars)))
  }

-- --- sim.resume -------------------------------------------------------------

toolSimResume :: Tool
toolSimResume = Tool
  { toolName = "sim.resume"
  , toolDescription = "Continue a timeline from its <basename>.state snapshot: generates N more candles with the snapshot's parameters and appends to the candles csv and tick sidecars."
  , toolSchema = schema
      [ ("out_path", "candles csv of the previous run", String "")
      , ("candles", "how many more candles to generate", Number 0) ]
  , toolRun = \ctx args -> catching $ do
      o <- asArgsIO args
      outPath <- reqStrIO o "out_path"
      n <- reqIntIO o "candles"
      inlineLimit <- optIntIO o "inline_limit" 10000
      st <- readState (statePath outPath) `catch` onMissing
      case st of
        Left err -> pure (Left err)
        Right (p0, done, gs) -> do
          let p = p0 { spCandles = n }
              hooks = simHooksFor ctx n
          sizes <- Run.newSidecarSizes
          Run.recordSidecarSizes outPath sizes
          res <- try (Run.appendCsvRes hooks outPath p done gs) :: IO (Either SomeException (GenState, Int))
          case res of
            Left err
              | Just SimCancelled <- fromException err -> do
                  Run.restoreSidecars outPath p sizes
                  pure (Right (object ["cancelled" .= True, "path" .= outPath]))
              | otherwise -> pure (Left (show err))
            Right (gs', added) -> do
              writeState (statePath outPath) p (done + added) gs'
              parsed <- MLD.loadBars outPath
              pure (case parsed of
                Left err -> Left err
                Right bars -> Right (seriesResult inlineLimit outPath (map MLD.barO bars) (map MLD.barH bars)
                                       (map MLD.barL bars) (map MLD.barC bars) (map MLD.barV bars)))
  }
  where
    onMissing e
      | isDoesNotExistError e = pure (Left "no state snapshot next to out_path")
      | otherwise             = pure (Left (show e))

-- --- indicators -------------------------------------------------------------

toolPermEntropy :: Tool
toolPermEntropy = indTool "ind.perm_entropy"
  "Rolling normalized permutation entropy (Bandt-Pompe, the KS proxy), 0..1 per candle, NaN warm-up."
  (\o closes -> do
      win <- optIntIO o "window" 240
      m <- optIntIO o "m" 5
      pure (Right (object ["values" .= rollingPermEntropy win m closes, "window" .= win, "m" .= m])))

toolCecp :: Tool
toolCecp = indTool "ind.cecp"
  "Rolling Complexity-Entropy Causality Plane over closes: per-candle (H, C) pairs."
  (\o closes -> do
      win <- optIntIO o "window" 240
      m <- optIntIO o "m" 4
      let pairs = rollingCECP win m closes
      pure (Right (object
        [ "h" .= map fst pairs
        , "c" .= map snd pairs
        , "window" .= win
        , "m" .= m
        ])))

toolFft :: Tool
toolFft = indTool "ind.fft"
  "Ridge-LS FFT fit + forecast over closes[start..end): returns the in-window fit and the forecast. edge_w > 1 endpoint-weights the fit."
  (\o closes -> do
      start <- reqIntIO o "start"
      end <- reqIntIO o "end"
      predict <- reqIntIO o "predict"
      waves <- optIntIO o "waves" 8
      edgeW <- optDoubleIO o "edge_w" 1.0
      let (fit, fc) = fftApprox waves edgeW closes start end predict
      if length fc /= predict
        then pure (Left "invalid window: forecast is empty")
        else pure (Right (object ["fit" .= fit, "forecast" .= fc, "n_fit" .= length fit])))

toolMarkov :: Tool
toolMarkov = indTool "ind.markov"
  "Bull/Sideways/Bear classification (window sums of close-to-close returns) plus the surprise-adaptive 3x3 Markov chain: per bar [bull, sw, bear, t00..t22, surprise, rolling surprise, lambda]."
  (\o closes -> do
      win <- reqIntIO o "period"
      surpWin <- optIntIO o "surpwin" 30
      th <- optDoubleIO o "thresh" 5.0
      lamHi <- optDoubleIO o "lam_hi" 0.99
      lamLo <- optDoubleIO o "lam_lo" 0.90
      sRef <- optDoubleIO o "s_ref" 0.55
      k <- optDoubleIO o "k" 4.0
      weighted <- optBoolIO o "weighted" False
      let mp = MarkovParams win surpWin th lamHi lamLo sRef k weighted
          states = statesOf mp closes
          vals = markovSeriesStates mp (VU.fromList (0 : VU.toList states))
      pure (Right (object
        [ "states" .= VU.toList states
        , "values" .= chunk15 (VU.toList states) vals
        ])))
  where
    -- re-associate the flat 15-per-bar output with the bars for readability
    chunk15 _ [] = []
    chunk15 (s : ss) vals = object ["state" .= s, "row" .= take 15 vals] : chunk15 ss (drop 15 vals)
    chunk15 [] _ = []

-- shared shape for closes-based indicator tools
indTool :: Text -> Text -> (Args -> [Double] -> IO (Either String Value)) -> Tool
indTool name desc go = Tool
  { toolName = name
  , toolDescription = desc
  , toolSchema = schema []
  , toolRun = \_ args -> catching $ do
      o <- asArgsIO args
      closesE <- getCloses o
      case closesE of
        Left err -> pure (Left err)
        Right closes -> go o closes
  }

-- --- ind.obvol --------------------------------------------------------------

toolObvol :: Tool
toolObvol = Tool
  { toolName = "ind.obvol"
  , toolDescription = "Run a tick-mode simulation and return OBVOL level lifetimes (placement candle, dissolution candle, side, price, peak volume) plus the sidecar paths."
  , toolSchema = schema [ ("params", "sim parameters object; mode must be a tick mode", Object KM.empty) ]
  , toolRun = \ctx args -> catching $ do
      o <- asArgsIO args
      pv <- reqIO o "params"
      p0 <- either (ioError . userError) pure (paramsFromJson pv)
      let p = case spMode p0 of
            ModeCandles -> p0 { spMode = ModeTickTrades }
            _           -> p0
      outPath <- resolveOut o "rel-mcp-obvol"
      let hooks = simHooksFor ctx (spCandles p)
      res <- try (Run.generateCsv hooks outPath p) :: IO (Either SomeException ())
      case res of
        Left err
          | Just SimCancelled <- fromException err -> do
              Run.cleanupOutput outPath p
              pure (Right (object ["cancelled" .= True]))
          | otherwise -> pure (Left (show err))
        Right _ -> do
          segs <- readLevelsCsv (levelsPath outPath)
          pure (Right (object
            [ "levels_path" .= levelsPath outPath
            , "ticks_path" .= ticksPath outPath
            , "candles_path" .= outPath
            , "segments" .= segs
            ]))
  }

-- --- strategy ---------------------------------------------------------------

toolDslBacktest :: Tool
toolDslBacktest = Tool
  { toolName = "strat.dsl_backtest"
  , toolDescription = "Backtest a hand-authored DSL strategy (priority rules COND -> POS, indicators close/sma(N), xup/xdn crosses) over a dataset; writes the optimizer artifact layout with seed 0 and returns the metrics."
  , toolSchema = schema
      [ ("data", "candles csv or .rbin (RELQ/RELS)", String "")
      , ("out", "results directory", String "")
      , ("dsl", "strategy text, rules separated by newlines", String "") ]
  , toolRun = \_ args -> catching $ do
      o <- asArgsIO args
      dataPath <- reqStrIO o "data"
      out <- reqStrIO o "out"
      specS <- reqStrIO o "dsl"
      fee <- optDoubleIO o "fee" 0.0002
      case STD.parseDsl specS of
        Left err -> pure (Left ("dsl parse: " ++ err))
        Right spec -> do
          Run.runDslBacktest dataPath out spec fee
          metricsJson (out ++ "/strat_0.json")
  }

toolStratOptimize :: Tool
toolStratOptimize = Tool
  { toolName = "strat.optimize"
  , toolDescription = "Random-search SMA-cross strategy optimization: N parallel sessions sampling slot strategies, ranked on validation Calmar, best re-run on held-out test. Writes per-session artifacts (strat_<seed>.json, equity/cand/test csv, trades_<seed>.bin)."
  , toolSchema = schema [ ("data", "candles csv or .rbin", String ""), ("out", "results directory", String "") ]
  , toolRun = \ctx args -> catching $ do
      o <- asArgsIO args
      dataPath <- reqStrIO o "data"
      out <- reqStrIO o "out"
      sessions <- optIntIO o "sessions" 4
      seedBase <- optIntIO o "seed" 1
      slots <- optIntIO o "slots" 4
      cands <- optIntIO o "candidates" 200
      valPct <- optDoubleIO o "valpct" 0.2
      fee <- optDoubleIO o "fee" 0.0002
      fastMax <- optIntIO o "fastmax" 50
      slowMax <- optIntIO o "slowmax" 200
      let opt = STO.defaultOptParams
            { STO.opSlots = slots, STO.opCandidates = cands
            , STO.opValFrac = valPct, STO.opFee = fee
            , STO.opFastMax = fastMax, STO.opSlowMax = slowMax }
          ns = max 1 sessions
      counter <- newIORef 0
      let total = ns * max 1 cands
          hooks = STO.OptHooks
            { STO.ohProgress = \_ _ -> bumpProgress ctx counter total
            , STO.ohBest = \_ -> pure ()
            , STO.ohCancelled = ctxCancelled ctx
            }
          seeds = [fromIntegral seedBase + fromIntegral k | k <- [0 .. ns - 1]]
      res <- try (Run.runStratBatch opt dataPath out (map (\s -> (s, hooks)) seeds))
        :: IO (Either SomeException ())
      case res of
        Left err | Just SimCancelled <- fromException err ->
          pure (Right (object ["cancelled" .= True, "out" .= out]))
        Left err -> pure (Left (show err))
        Right _ -> do
          ms <- mapM (metricsJson' (out ++ "/strat_") ) seeds
          pure (Right (object ["out" .= out, "sessions" .= ms]))
  }

toolTrades :: Tool
toolTrades = Tool
  { toolName = "strat.trades"
  , toolDescription = "Decode a trades_<seed>.bin (RELT) into a list of round-trip trades with entry/exit, pnl, volume and cause."
  , toolSchema = schema [ ("path", "path to a RELT trades binary", String "") ]
  , toolRun = \_ args -> catching $ do
      o <- asArgsIO args
      path <- reqStrIO o "path"
      bs <- B.readFile path
      case parseTradesBin bs of
        Left err -> pure (Left err)
        Right ts -> pure (Right (object ["trades" .= map tradeJson ts]))
  }

tradeJson :: STB.Trade -> Value
tradeJson t = object
  [ "i0" .= STB.trI0 t
  , "i1" .= STB.trI1 t
  , "side" .= STB.trSide t
  , "cause" .= STB.trCause t
  , "entry" .= STB.trEntry t
  , "exit" .= STB.trExit t
  , "pnl" .= STB.trPnl t
  , "vol" .= STB.trVol t
  ]

-- --- ML ---------------------------------------------------------------------

toolMlTrain :: Tool
toolMlTrain = Tool
  { toolName = "ml.train"
  , toolDescription = "Train a batch of parallel MLP sessions on a candles dataset (features: lags/vol/range/SMA/volume + optional permutation entropy; label = next-candle direction), selected on validation Calmar, evaluated on held-out test. Writes per-session artifacts (ml_<seed>.json, equity/hist/test csv)."
  , toolSchema = schema [ ("data", "candles csv or .rbin", String ""), ("out", "results directory", String "") ]
  , toolRun = \ctx args -> catching $ do
      o <- asArgsIO args
      dataPath <- reqStrIO o "data"
      out <- reqStrIO o "out"
      sessions <- optIntIO o "sessions" 4
      seedBase <- optIntIO o "seed" 1
      epochs <- optIntIO o "epochs" 50
      lr <- optDoubleIO o "lr" 0.05
      batch <- optIntIO o "batch" 256
      hiddenS <- maybe (pure Nothing) (fmap Just . argTextIO) (KM.lookup "hidden" o)
      hidden <- either (ioError . userError) pure (parseIntList hiddenS)
      trainPct <- optDoubleIO o "trainpct" 0.6
      valPct <- optDoubleIO o "valpct" 0.2
      fee <- optDoubleIO o "fee" 0.0002
      lagsS <- maybe (pure Nothing) (fmap Just . argTextIO) (KM.lookup "lags" o)
      lags <- either (ioError . userError) pure (parseIntList lagsS)
      volWin <- optIntIO o "volwin" 20
      rangeWin <- optIntIO o "rangewin" 20
      smaWin <- optIntIO o "smawin" 20
      volSma <- optIntIO o "volsma" 20
      ksWin <- optIntIO o "kswin" 0
      ksM <- optIntIO o "ksm" 5
      labelEps <- optDoubleIO o "labeleps" 1e-5
      let spec = MLD.defaultFeatureSpec
            { MLD.fsLags = fromMaybe [1, 5, 20] lags
            , MLD.fsVolWin = volWin, MLD.fsRangeWin = rangeWin
            , MLD.fsSmaWin = smaWin, MLD.fsVolSmaWin = volSma
            , MLD.fsKsWin = ksWin, MLD.fsKsM = ksM, MLD.fsLabelEps = labelEps
            }
          tp = MLT.TrainParams epochs lr batch (fromMaybe [16] hidden) trainPct valPct fee spec
          ns = max 1 sessions
      counter <- newIORef 0
      let total = ns * max 1 epochs
          hooks = MLT.TrainHooks
            { MLT.thProgress = \_ _ -> bumpProgress ctx counter total
            , MLT.thBest = \_ -> pure ()
            , MLT.thCancelled = ctxCancelled ctx
            }
          seeds = [fromIntegral seedBase + fromIntegral k | k <- [0 .. ns - 1]]
      res <- try (Run.runMlBatch tp dataPath out (map (\s -> (s, hooks)) seeds))
        :: IO (Either SomeException ())
      case res of
        Left err | Just SimCancelled <- fromException err ->
          pure (Right (object ["cancelled" .= True, "out" .= out]))
        Left err -> pure (Left (show err))
        Right _ -> do
          ms <- mapM (metricsJson' (out ++ "/ml_") ) seeds
          pure (Right (object ["out" .= out, "sessions" .= ms]))
  }

-- --- data -------------------------------------------------------------------

toolReadSeries :: Tool
toolReadSeries = Tool
  { toolName = "data.read_series"
  , toolDescription = "Load a candles csv (sim or real-quotes format) or a binary .rbin store (RELQ/RELS) and return OHLCV series — inline arrays up to limit, otherwise a summary plus the path."
  , toolSchema = schema [ ("path", "candles csv or .rbin", String "") ]
  , toolRun = \_ args -> catching $ do
      o <- asArgsIO args
      path <- reqStrIO o "path"
      limit <- optIntIO o "limit" 10000
      parsed <- MLD.loadBars path
      pure (case parsed of
        Left err -> Left err
        Right bars -> Right (seriesResult limit path
               (map MLD.barO bars) (map MLD.barH bars) (map MLD.barL bars)
               (map MLD.barC bars) (map MLD.barV bars)))
  }

toolListFiles :: Tool
toolListFiles = Tool
  { toolName = "data.list_files"
  , toolDescription = "List files in a directory, optionally filtered by a substring suffix (e.g. .csv, .rbin)."
  , toolSchema = schema [ ("dir", "directory path", String "") ]
  , toolRun = \_ args -> catching $ do
      o <- asArgsIO args
      dir <- reqStrIO o "dir"
      suffix <- maybe (pure "") argTextIO (KM.lookup "suffix" o)
      limit <- optIntIO o "limit" 200
      names <- sort <$> listDirectory dir
      let matched = filter (\n -> null suffix || suffix `isSuffixOf` n) names
      pure (Right (object
        [ "dir" .= dir
        , "files" .= take limit matched
        , "count" .= length matched
        ]))
  }

-- --- shared result shapes ---------------------------------------------------

-- OHLCV payload: full arrays up to inlineLimit rows, otherwise a summary
seriesResult :: Int -> FilePath -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Value
seriesResult inlineLimit path os hs ls cs vs
  | length cs <= inlineLimit = object
      [ "path" .= path
      , "count" .= length cs
      , "open" .= os, "high" .= hs, "low" .= ls, "close" .= cs, "vol" .= vs
      ]
  | otherwise = object
      [ "path" .= path
      , "count" .= length cs
      , "inline" .= False
      , "first" .= head cs
      , "last" .= last cs
      , "min" .= minimum ls
      , "max" .= maximum hs
      ]

metricsJson :: FilePath -> IO (Either String Value)
metricsJson path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("missing metrics file: " ++ path))
    else do
      txt <- readFile path
      pure (maybe (Left ("bad metrics json: " ++ path)) Right (decode (BL8.pack txt)))

metricsJson' :: FilePath -> Word64 -> IO Value
metricsJson' prefix seed = do
  r <- metricsJson (prefix ++ show seed ++ ".json")
  pure (either (const (object ["seed" .= seed])) id r)

-- parse the levels csv written by a tick-mode run into segment objects
readLevelsCsv :: FilePath -> IO [Value]
readLevelsCsv path = do
  txt <- readFile path
  pure (map seg (drop 1 (lines txt)))
  where
    seg ln = case splitCommas ln of
      (c0 : c1 : side : px : peak : _) -> object
        [ "c0" .= (readDef 0 c0 :: Int)
        , "c1" .= (readDef 0 c1 :: Int)
        , "side" .= (if side == "0" then ("bid" :: Text) else "ask")
        , "price" .= (readDef 0 px :: Double)
        , "peak" .= (readDef 0 peak :: Int)
        ]
      _ -> object []
    splitCommas s = case break (== ',') s of
      (a, [])     -> [a]
      (a, _ : rest) -> a : splitCommas rest
    readDef d s = case reads s of
      [(x, "")] -> x
      _         -> d

-- --- small helpers ----------------------------------------------------------

resolveOut :: Args -> String -> IO FilePath
resolveOut o stem = case KM.lookup "out_path" o of
  Just (String p) -> pure (T.unpack p)
  Just _          -> pure ("/tmp/" ++ stem ++ ".csv")
  Nothing         -> do
    tmp <- getTemporaryDirectory
    pure (tmp ++ "/" ++ stem ++ ".csv")

simHooksFor :: ToolCtx -> Int -> SimHooks
simHooksFor ctx total = SimHooks
  { hookProgress = \d t -> when (d == t || d `mod` step == 0) $
      ctxProgress ctx (fromIntegral d / fromIntegral (t + 1))
  , hookCancelled = ctxCancelled ctx
  }
  where
    step = max 1 (total `div` 50)

bumpProgress :: ToolCtx -> IORef Int -> Int -> IO ()
bumpProgress ctx ref total = do
  d <- atomicModifyIORef' ref (\x -> (x + 1, x + 1))
  ctxProgress ctx (fromIntegral d / fromIntegral total)

argText :: Value -> Either String String
argText (String s) = Right (T.unpack s)
argText _          = Left "expected a string"

argTextIO :: Value -> IO String
argTextIO = either (ioError . userError) pure . argText

parseIntList :: Maybe String -> Either String (Maybe [Int])
parseIntList Nothing = Right Nothing
parseIntList (Just s) = case mapM readOne (splitCommas s) of
  Just is | not (null is) -> Right (Just is)
  _                       -> Left ("bad int list: " ++ s)
  where
    readOne v = case reads v of
      [(x, "")] -> Just x
      _         -> Nothing
    splitCommas str = case break (== ',') str of
      (a, [])       -> [a]
      (a, _ : rest) -> a : splitCommas rest
