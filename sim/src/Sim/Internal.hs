{-# LANGUAGE StrictData #-}

module Sim.Internal
  ( Price (..)
  , Vol (..)
  , mkVol
  , unVol
  , Side (..)
  , Order (..)
  , Book (..)
  , emptyBook
  , Trade (..)
  , Candle (..)
  , emptyCandle
  , isEmptyCandle
  , SimMode (..)
  , PriceOffset (..)
  , SimParams (..)
  , defaultParams
  , Tick (..)
  , SimHooks (..)
  , defaultHooks
  , SimCancelled (..)
  ) where

import Control.Exception (Exception)
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

newtype Price = Price { unPrice :: Int }
  deriving (Eq, Ord, Show)

-- volume is positive by construction; use mkVol to build one
newtype Vol = Vol { unVol :: Int }
  deriving (Eq, Ord, Show)

mkVol :: Int -> Maybe Vol
mkVol n
  | n > 0     = Just (Vol n)
  | otherwise = Nothing

data Side = Bid | Ask
  deriving (Eq, Ord, Show)

data Order = Order
  { orderSide  :: !Side
  , orderPrice :: !Price
  , orderVol   :: !Vol
  }
  deriving (Show)

data Book = Book
  { bookBids :: !(Map.Map Price Vol)
  , bookAsks :: !(Map.Map Price Vol)
  }
  deriving (Show)

emptyBook :: Book
emptyBook = Book Map.empty Map.empty

data Trade = Trade
  { tradePrice :: !Price
  , tradeVol   :: !Vol
  }
  deriving (Show)

data Candle = Candle
  { candleOpen  :: !Price
  , candleHigh  :: !Price
  , candleLow   :: !Price
  , candleClose :: !Price
  , candleVol   :: !Int
  }
  deriving (Eq, Show)

emptyCandle :: Candle
emptyCandle = Candle (Price 0) (Price 0) (Price 0) (Price 0) 0

isEmptyCandle :: Candle -> Bool
isEmptyCandle = (== Price 0) . candleOpen

data SimMode = ModeCandles | ModeTickTrades | ModeTickSteps
  deriving (Eq, Show)

-- how an order's distance from its reference price is measured:
-- ticks = arithmetic (pd ticks), percent = proportional (pd basis points
-- of the reference price)
data PriceOffset = OffsetTicks | OffsetPercent
  deriving (Eq, Show)

data SimParams = SimParams
  { spSeed            :: !Word64
  , spCandles         :: !Int
  , spOrdersLo        :: !Int
  , spOrdersHi        :: !Int
  , spTickRange       :: !Int
  , spVolLo           :: !Int
  , spVolHi           :: !Int
  , spStartBid        :: !Price
  , spStartAsk        :: !Price
  , spStartVol        :: !Vol
  , spBuyThresh       :: !Double
  , spSellThresh      :: !Double
  , spPriceScale      :: !Int
  , spMode            :: !SimMode
  , spBookDepth       :: !Int
  , spClampRest       :: !Bool
  , spPriceOffset     :: !PriceOffset
  , spZeroFloor       :: !Bool
  , spSinus           :: !Bool
  , spSinusPeriod     :: !Int
  , spSinusAmp        :: !Int
  , spSinusWaves      :: !Int
  , spSinusVolBase    :: !Int
  , spSinusVolAmp     :: !Int
  }
  deriving (Show)

defaultParams :: SimParams
defaultParams = SimParams
  { spSeed            = 42
  , spCandles         = 1000
  , spOrdersLo        = 500
  , spOrdersHi        = 500
  , spTickRange       = 10
  , spVolLo           = 1
  , spVolHi           = 20
  , spStartBid        = Price 102190
  , spStartAsk        = Price 102195
  , spStartVol        = Vol 200
  , spBuyThresh       = 0.25
  , spSellThresh      = 0.75
  , spPriceScale      = 100000
  , spMode            = ModeCandles
  , spBookDepth       = 50
  , spClampRest       = False
  , spPriceOffset     = OffsetTicks
  , spZeroFloor       = False
  , spSinus           = False
  , spSinusPeriod     = 100
  , spSinusAmp        = 500
  , spSinusWaves      = 1
  , spSinusVolBase    = 1000
  , spSinusVolAmp     = 400
  }

-- one row of the ticks export: a trade (tickVol > 0) or a book-only
-- update (tickVol == 0), plus the top of the book right after the event;
-- levels are (price, vol) pairs, bids descending, asks ascending
data Tick = Tick
  { tickCandle :: !Int
  , tickPrice  :: !Price
  , tickVol    :: !Int
  , tickBids   :: ![(Price, Vol)]
  , tickAsks   :: ![(Price, Vol)]
  }
  deriving (Show)

-- per-candle hooks for driving a UI: progress (candles done, total) and
-- a cancellation poll, checked once per candle by the hooked runners
data SimHooks = SimHooks
  { hookProgress  :: Int -> Int -> IO ()
  , hookCancelled :: IO Bool
  }

defaultHooks :: SimHooks
defaultHooks = SimHooks (\_ _ -> pure ()) (pure False)

-- thrown by the hooked runners when hookCancelled reports True
data SimCancelled = SimCancelled
  deriving Show

instance Exception SimCancelled
