module Sonowz.StockNoti.Stock.Logic.Cross
  ( calcGoldenCross,
    calcDeadCross,
  )
where

import Data.Time (UTCTime)
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockPrice (..), StockTimeSeries (..))

data TimeSeries value = TimeSeries
  { sTime :: UTCTime,
    sValue :: value
  }
  deriving (Eq, Show)

calcGoldenCross :: Int -> Int -> StockTimeSeries tu -> [UTCTime]
calcGoldenCross = calcCross isGoldenCross
  where
    isGoldenCross x y = uncurry (<) x && uncurry (>=) y

calcDeadCross :: Int -> Int -> StockTimeSeries tu -> [UTCTime]
calcDeadCross = calcCross isDeadCross
  where
    isDeadCross x y = uncurry (>) x && uncurry (<=) y

type CrossFunction = (Double, Double) -> (Double, Double) -> Bool

calcCross :: CrossFunction -> Int -> Int -> StockTimeSeries tu -> [UTCTime]
calcCross isCross smaPeriodShort smaPeriodLong stockTimeSeries = calcCross' zippedSma
  where
    closePrices = (\sp -> TimeSeries (time sp) (close sp)) <$> prices stockTimeSeries
    smaShort = calcSMA smaPeriodShort closePrices
    smaLong = calcSMA smaPeriodLong closePrices
    zippedSma = mapMaybe (\(TimeSeries t l) -> (\ts -> TimeSeries t (sValue ts, l)) <$> find (\ts -> sTime ts == t) smaShort) smaLong

    calcCross' :: [TimeSeries (Double, Double)] -> [UTCTime]
    calcCross' [] = []
    calcCross' [_] = []
    calcCross' (x : y : xs) =
      if isCross (sValue x) (sValue y)
        then sTime y : calcCross' (y : xs)
        else calcCross' (y : xs)

calcSMA :: Int -> [TimeSeries Double] -> [TimeSeries Double]
calcSMA period prices = snd $ mapAccumL accumFn initWindow prices'
  where
    accumFn :: [TimeSeries Double] -> TimeSeries Double -> ([TimeSeries Double], TimeSeries Double)
    accumFn window value = (drop 1 newWindow, sma)
      where
        newWindow = window <> [value]
        sma = TimeSeries (sTime value) (sum (sValue <$> newWindow) / fromIntegral period)
    (initWindow, prices') = splitAt (period - 1) prices
