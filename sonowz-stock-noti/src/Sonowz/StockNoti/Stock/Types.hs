module Sonowz.StockNoti.Stock.Types
  ( TimeUnit (..),
    StockSymbol (..),
    StockTimeSeries (..),
    StockPrice (..),
    getTimeSeries,
  )
where

import Data.Time (UTCTime)
import Sonowz.StockNoti.Imports

data TimeUnit = TYear | TMonth | TWeek | TDay | THour deriving (Eq, Show)

newtype StockSymbol = StockSymbol Text deriving (Eq, Show) via Text

data StockTimeSeries (tu :: TimeUnit) = StockTimeSeries
  { symbol :: StockSymbol,
    name :: Text,
    prices :: [StockPrice tu]
  }
  deriving (Eq, Show)

data StockPrice (tu :: TimeUnit) = StockPrice
  { time :: UTCTime,
    open :: Double,
    close :: Double,
    volume :: Double
  }
  deriving (Eq, Show)

getTimeSeries :: (StockPrice tu -> Double) -> StockTimeSeries tu -> [Double]
getTimeSeries field = fmap field . prices
