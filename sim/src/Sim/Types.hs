{-# LANGUAGE StrictData #-}

module Sim.Types where

import Data.Word (Word64)
import qualified Data.Map.Strict as Map

newtype Price = Price { unPrice :: Int }
  deriving (Eq, Ord, Show)

newtype Vol = Vol { unVol :: Int }
  deriving (Eq, Ord, Show)

data Book = Book
  { bookBids :: !(Map.Map Price Vol)
  , bookAsks :: !(Map.Map Price Vol)
  }
  deriving (Show)

emptyBook :: Book
emptyBook = Book Map.empty Map.empty

data Candle = Candle
  { candleOpen  :: !Price
  , candleHigh  :: !Price
  , candleLow   :: !Price
  , candleClose :: !Price
  }
  deriving (Show)

emptyCandle :: Candle
emptyCandle = Candle (Price 0) (Price 0) (Price 0) (Price 0)

isEmptyCandle :: Candle -> Bool
isEmptyCandle = (== Price 0) . candleOpen

data Tick = Tick
  { tickPrice :: !Price
  , tickVol   :: !Vol
  }
  deriving (Show)

data SimParams = SimParam
  { spSeed            :: !Word64
  , spCandles         :: !Int
  , spOrdersPerCandle :: !Int
  , spTickRange       :: !Int
  , spVolLo           :: !Int
  , spVolHi           :: !Int
  , spStartBid        :: !Price
  , spStartAsk        :: !Price
  , spStartVol        :: !Vol
  , spBuyThresh       :: !Double
  , spSellThresh      :: !Double
  , spPriceScale      :: !Int
  }
  deriving (Show)

defaultParams :: SimParams
defaultParams = SimParam
  { spSeed            = 42
  , spCandles         = 1000
  , spOrdersPerCandle = 500
  , spTickRange       = 10
  , spVolLo           = 1
  , spVolHi           = 20
  , spStartBid        = Price 102190
  , spStartAsk        = Price 102195
  , spStartVol        = Vol 200
  , spBuyThresh       = 0.25
  , spSellThresh      = 0.75
  , spPriceScale      = 100000
  }
