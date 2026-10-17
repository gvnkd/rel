{-# LANGUAGE StrictData #-}

-- textual DSL for hand-authored strategies, an alternative to the
-- fixed-slot search space of Sim.Strat.Rules. A strategy is a priority
-- list of rules "COND -> POS": at each candle the first rule whose
-- condition holds sets the position (-1/0/+1), no match = flat. The
-- level-based semantics is stateless — different from the slot latch,
-- which keeps a position until the next opposite cross.
--
-- grammar (one rule per line; ';' also separates rules):
--   rule  := cond '->' int
--   cond  := or                          ('|' lowest, '&' tighter, '!' tightest)
--   or    := and ('|' and)*
--   and   := not ('& not)*
--   not   := '!' not | cmp | xup/xdn | '(' cond ')'
--   cmp   := ind rel ind                 rel: > >= < <=
--   xup/xdn := 'xup(' ind ',' ind ')'    cross up / down (NaN->side counts)
--   ind   := 'close' | 'sma(' int ')'
--
-- Text round-trips through parseDsl/dslText for the FFI params string.
module Sim.Strat.Dsl
  ( Ind (..)
  , Rel (..)
  , Cond (..)
  , Rule (..)
  , DslStrat (..)
  , parseDsl
  , dslText
  , dslSignal
  , dslSignalCause
  , dslMaxPeriod
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Sim.Strat.Backtest (cmpD, smaSeries)

data Ind
  = Px
  | Sma !Int
  deriving (Eq, Ord, Show)

data Rel = RGt | RGe | RLt | RLe deriving (Eq, Show)

relSatisfies :: Rel -> Ordering -> Bool
relSatisfies RGt = (== GT)
relSatisfies RGe = (/= LT)
relSatisfies RLt = (== LT)
relSatisfies RLe = (/= GT)

relShow :: Rel -> String
relShow RGt = ">"
relShow RGe = ">="
relShow RLt = "<"
relShow RLe = "<="

data Cond
  = Cmp !Rel !Ind !Ind
  | CrossUp !Ind !Ind
  | CrossDown !Ind !Ind
  | AndC !Cond !Cond
  | OrC !Cond !Cond
  | NotC !Cond
  deriving (Eq, Show)

data Rule = Rule
  { rCond :: !Cond
  , rPos  :: !Int
  } deriving (Eq, Show)

newtype DslStrat = DslStrat { dsRules :: [Rule] }
  deriving (Eq, Show)

-- --- evaluation ------------------------------------------------------------

indSeries :: Ind -> VU.Vector Double -> VU.Vector Double
indSeries Px = id
indSeries (Sma w) = smaSeries w

-- per-candle positions. Indicator series and cross diffs are built once
-- per distinct pair; conditions are then O(1) per candle per rule.
dslSignal :: DslStrat -> VU.Vector Double -> VU.Vector Int
dslSignal st closes = fst (dslSignalCause st closes)

-- signal + per-candle cause (the text of the first matching rule)
dslSignalCause :: DslStrat -> VU.Vector Double -> (VU.Vector Int, V.Vector String)
dslSignalCause (DslStrat rules) closes
  | VU.null closes = (VU.empty, V.empty)
  | null rules = (VU.replicate (VU.length closes) 0, V.replicate (VU.length closes) "")
  | otherwise =
      let n = VU.length closes
          conds = map rCond rules
          allInds = nub (concatMap condInds conds)
          crossPs = nub (concatMap condCrossPairs conds)
          seriesOf = Map.fromList [ (ind, indSeries ind closes) | ind <- allInds ]
          diffsOf = Map.fromList
            [ (k, VU.zipWith (-) (seriesOf Map.! fst k) (seriesOf Map.! snd k))
            | k <- crossPs ]
          causeOf r = showCond (rCond r) ++ " -> " ++ show (rPos r)
          ruleVecs = [ (VU.generate n (condAt (rCond r)), rPos r, causeOf r)
                     | r <- rules ]
          condAt c i = case c of
            Cmp rel a b ->
              let x = seriesOf Map.! a VU.! i
                  y = seriesOf Map.! b VU.! i
              in not (isNaN x || isNaN y)
                 && relSatisfies rel (compare x y)
            CrossUp a b -> crossTo GT (diffsOf Map.! (a, b)) i
            CrossDown a b -> crossTo LT (diffsOf Map.! (a, b)) i
            AndC a b -> condAt a i && condAt b i
            OrC a b -> condAt a i || condAt b i
            NotC a -> not (condAt a i)
          crossTo side d i =
            i >= 1
            && cmpD (d VU.! i) == side
            && cmpD (d VU.! (i - 1)) /= side
          signalAt i = go ruleVecs
            where
              go ((v, p, _) : rest) = if v VU.! i then p else go rest
              go [] = 0
          causeAt i = go ruleVecs
            where
              go ((v, _, c) : rest) = if v VU.! i then c else go rest
              go [] = ""
      in (VU.generate n signalAt, V.generate n causeAt)
  where
    condInds c = case c of
      Cmp _ a b     -> [a, b]
      CrossUp a b   -> [a, b]
      CrossDown a b -> [a, b]
      AndC a b      -> condInds a ++ condInds b
      OrC a b       -> condInds a ++ condInds b
      NotC a        -> condInds a
    condCrossPairs c = case c of
      CrossUp a b   -> [(a, b)]
      CrossDown a b -> [(a, b)]
      AndC a b      -> condCrossPairs a ++ condCrossPairs b
      OrC a b       -> condCrossPairs a ++ condCrossPairs b
      NotC a        -> condCrossPairs a
      _             -> []

-- slowest SMA period anywhere in the strategy (warm-up bound)
dslMaxPeriod :: DslStrat -> Int
dslMaxPeriod (DslStrat rules) = maximum (1 : concatMap rulePeriods rules)
  where
    rulePeriods r = condPeriods (rCond r)
    condPeriods c = case c of
      Cmp _ a b     -> indPeriods a ++ indPeriods b
      CrossUp a b   -> indPeriods a ++ indPeriods b
      CrossDown a b -> indPeriods a ++ indPeriods b
      AndC a b      -> condPeriods a ++ condPeriods b
      OrC a b       -> condPeriods a ++ condPeriods b
      NotC a        -> condPeriods a
    indPeriods Px = []
    indPeriods (Sma w) = [w]

-- --- text form -------------------------------------------------------------

dslText :: DslStrat -> String
dslText (DslStrat rules) =
  intercalate "\n" [ showCond (rCond r) ++ " -> " ++ show (rPos r) | r <- rules ]

showInd :: Ind -> String
showInd Px = "close"
showInd (Sma w) = "sma(" ++ show w ++ ")"

showCond :: Cond -> String
showCond c = case c of
  Cmp rel a b -> showInd a ++ relShow rel ++ showInd b
  CrossUp a b -> "xup(" ++ showInd a ++ "," ++ showInd b ++ ")"
  CrossDown a b -> "xdn(" ++ showInd a ++ "," ++ showInd b ++ ")"
  NotC a -> "!" ++ showTight a
  AndC a b -> showSide a ++ " & " ++ showSide b
  OrC a b -> showCond a ++ " | " ++ showCond b
  where
    -- argument printers: '&' args must parenthesize '|' children
    showSide cc = case cc of
      OrC _ _ -> "(" ++ showCond cc ++ ")"
      _       -> showCond cc
    -- '!' args must parenthesize boolean children
    showTight cc = case cc of
      AndC _ _ -> "(" ++ showCond cc ++ ")"
      OrC _ _  -> "(" ++ showCond cc ++ ")"
      _        -> showCond cc

parseDsl :: String -> Either String DslStrat
parseDsl s = do
  let lns = filter (not . null) (map trim (splitRules s))
  if null lns
    then Left "no rules"
    else DslStrat <$> mapM parseRule lns
  where
    splitRules = foldr sep [] . split '\n'
    sep chunk acc = case split ';' chunk of
      [one] -> trim one : acc
      many  -> map trim many ++ acc

parseRule :: String -> Either String Rule
parseRule s = do
  toks <- tokenize s
  (c, rest) <- parseCond toks
  case rest of
    [TArrow, TNum p]
      | p >= -1 && p <= 1 -> Right (Rule c p)
      | otherwise -> Left ("position out of range in: " ++ s)
    _ -> Left ("bad rule (expected 'COND -> POS'): " ++ s)

-- --- tokenizer / parser ----------------------------------------------------

data Tok
  = TIdent String
  | TNum Int
  | TLP
  | TRP
  | TComma
  | TArrow
  | TRel Rel
  | TAnd
  | TOr
  | TNot
  deriving (Eq, Show)

tokenize :: String -> Either String [Tok]
tokenize [] = Right []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | c == '(' = (TLP :) <$> tokenize cs
  | c == ')' = (TRP :) <$> tokenize cs
  | c == ',' = (TComma :) <$> tokenize cs
  | c == '&' = (TAnd :) <$> tokenize cs
  | c == '|' = (TOr :) <$> tokenize cs
  | c == '!' = (TNot :) <$> tokenize cs
  | c == '-' , ('>' : more) <- cs = (TArrow :) <$> tokenize more
  | c == '-' , (d : more) <- cs, isDigit d =
      let (ds, rest) = span isDigit (d : more)
      in (TNum (negate (read ds)) :) <$> tokenize rest
  | c == '>' , ('=' : more) <- cs = (TRel RGe :) <$> tokenize more
  | c == '>' = (TRel RGt :) <$> tokenize cs
  | c == '<' , ('=' : more) <- cs = (TRel RLe :) <$> tokenize more
  | c == '<' = (TRel RLt :) <$> tokenize cs
  | isDigit c =
      let (ds, more) = span isDigit (c : cs)
      in (TNum (read ds) :) <$> tokenize more
  | isAlphaNum c =
      let (w, more) = span isAlphaNum (c : cs)
      in (TIdent w :) <$> tokenize more
  | otherwise = Left ("unexpected character: " ++ [c])

parseCond :: [Tok] -> Either String (Cond, [Tok])
parseCond = parseOr

parseOr :: [Tok] -> Either String (Cond, [Tok])
parseOr ts = do
  (a, rest) <- parseAnd ts
  go a rest
  where
    go a (TOr : more) = do
      (b, rest) <- parseAnd more
      go (OrC a b) rest
    go a rest = Right (a, rest)

parseAnd :: [Tok] -> Either String (Cond, [Tok])
parseAnd ts = do
  (a, rest) <- parseNot ts
  go a rest
  where
    go a (TAnd : more) = do
      (b, rest) <- parseNot more
      go (AndC a b) rest
    go a rest = Right (a, rest)

parseNot :: [Tok] -> Either String (Cond, [Tok])
parseNot (TNot : ts) = do
  (c, rest) <- parseNot ts
  pure (NotC c, rest)
parseNot ts = parseAtom ts

parseAtom :: [Tok] -> Either String (Cond, [Tok])
parseAtom (TLP : ts) = do
  (c, rest) <- parseCond ts
  case rest of
    TRP : more -> Right (c, more)
    _          -> Left "missing )"
parseAtom (TIdent f : TLP : ts) | f == "xup" || f == "xdn" = do
  (a, rest1) <- parseInd ts
  case rest1 of
    TComma : more1 -> do
      (b, rest2) <- parseInd more1
      case rest2 of
        TRP : more2 | f == "xup" -> Right (CrossUp a b, more2)
                    | otherwise  -> Right (CrossDown a b, more2)
        _ -> Left "missing ) after cross arguments"
    _ -> Left "missing , in cross"
parseAtom ts = do
  (a, rest) <- parseInd ts
  case rest of
    TRel rel : more -> do
      (b, rest2) <- parseInd more
      pure (Cmp rel a b, rest2)
    _ -> Left "expected a relation (> >= < <=)"

parseInd :: [Tok] -> Either String (Ind, [Tok])
parseInd (TIdent "close" : rest) = Right (Px, rest)
parseInd (TIdent "sma" : TLP : TNum n : TRP : rest)
  | n >= 1 = Right (Sma n, rest)
  | otherwise = Left "sma period must be >= 1"
parseInd _ = Left "expected an indicator (close or sma(N))"

-- --- small string helpers (local copies; keeps the module FFI-free) --------

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (pre, [])       -> [pre]
  (pre, _ : post) -> pre : split c post

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
