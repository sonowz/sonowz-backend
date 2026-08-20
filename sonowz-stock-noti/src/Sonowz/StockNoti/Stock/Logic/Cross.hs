module Sonowz.StockNoti.Stock.Logic.Cross
  ( calcGoldenCross,
    calcDeadCross,
  )
where

import Data.Time (UTCTime)
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockPrice (..), StockTimeSeries (..))

data TimeSeries value = TimeSeries
  { time :: UTCTime,
    value :: value
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
    closePrices = (\sp -> TimeSeries sp.time sp.close) <$> stockTimeSeries.prices
    smaShort = calcSMA smaPeriodShort closePrices
    smaLong = calcSMA smaPeriodLong closePrices
    zippedSma = mapMaybe (\(TimeSeries t l) -> (\ts -> TimeSeries t (ts.value, l)) <$> find (\ts -> ts.time == t) smaShort) smaLong

    calcCross' :: [TimeSeries (Double, Double)] -> [UTCTime]
    calcCross' [] = []
    calcCross' [_] = []
    calcCross' (x : y : xs) =
      if isCross x.value y.value
        then y.time : calcCross' (y : xs)
        else calcCross' (y : xs)

calcSMA :: Int -> [TimeSeries Double] -> [TimeSeries Double]
calcSMA period prices = snd $ mapAccumL accumFn initWindow prices'
  where
    accumFn :: [TimeSeries Double] -> TimeSeries Double -> ([TimeSeries Double], TimeSeries Double)
    accumFn window value = (drop 1 newWindow, sma)
      where
        newWindow = window <> [value]
        sma = TimeSeries value.time (sum ((.value) <$> newWindow) / fromIntegral period)
    (initWindow, prices') = splitAt (period - 1) prices
