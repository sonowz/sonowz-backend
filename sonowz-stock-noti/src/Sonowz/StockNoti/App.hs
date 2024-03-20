module Sonowz.StockNoti.App
  ( runApp,
  )
where

import Control.Concurrent (threadDelay)
import Data.Time (UTCTime (utctDay))
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.StockNoti.Env (Env (..))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Notification (StockNotificationType (NotiDeadCross), createNotification)
import Sonowz.StockNoti.Stock.DataSource.AlphaVantage (runStockDataSourceAlphaVantage)
import Sonowz.StockNoti.Stock.DataSource.Effect (StockDataSource, fetchDayStockPrices)
import Sonowz.StockNoti.Stock.Logic.Cross (calcDeadCross)
import Sonowz.StockNoti.Stock.Types (StockSymbol)

runApp :: HasCallStack => Env -> IO Void
runApp env = infinitely $ do
  mainLoopIO
  threadDelay' (fromIntegral (envWorkerIntervalSeconds env) * 10 ^ 6)
  where
    -- 'threadDelay' with Integer parameter
    threadDelay' :: Integer -> IO ()
    threadDelay' t
      | t > 10 ^ 9 = threadDelay (10 ^ 9) >> threadDelay' (t - 10 ^ 9)
      | otherwise = threadDelay (fromIntegral t)

    mainLoopIO :: IO ()
    mainLoopIO =
      mainLoop (envStockSymbols env)
        & runStockDataSourceAlphaVantage
        & runReader (envPgConnection env)
        & (runError @ParseException >=> either logException pure)
        & embedToFinal
        & resourceToIOFinal
        & stdEffToIOFinal
        & runFinal @IO

type MainLoopEffects = Final IO : StockDataSource : DBEffects

mainLoop :: (HasCallStack, Members MainLoopEffects r) => [StockSymbol] -> Sem r ()
mainLoop stocks = forM_ stocks $ \stock -> runError @SomeException . flip catch logException . fromExceptionSem $ do
  logInfo $ "Checking " <> show stock <> "..."
  stockTimeSeries <- fetchDayStockPrices stock
  let smaPeriodShort = 5
      smaPeriodLong = 18
      deadCrossTimes = calcDeadCross smaPeriodShort smaPeriodLong stockTimeSeries
  case nonEmpty deadCrossTimes of
    Nothing -> logDebug $ show stock <> " has no dead cross."
    Just xs ->
      let lastDate = utctDay (last xs)
       in void $ createNotification stock NotiDeadCross lastDate
