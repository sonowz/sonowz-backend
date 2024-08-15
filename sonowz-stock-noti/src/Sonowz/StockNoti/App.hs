module Sonowz.StockNoti.App
  ( runApp,
  )
where

import Data.Time (UTCTime (utctDay))
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Error.Effect (catchAnyException, foreverCatch, runErrorAsLogging)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.Time.Effect (threadDelay, timeToIO)
import Sonowz.StockNoti.Env (Env (..))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Notification (StockNotificationType (NotiDeadCross, NotiGoldenCross), createNotification)
import Sonowz.StockNoti.Stock.DataSource.AlphaVantage (runStockDataSourceAlphaVantage)
import Sonowz.StockNoti.Stock.DataSource.Effect (StockDataSource, fetchDayStockPrices)
import Sonowz.StockNoti.Stock.Logic.Cross (calcDeadCross, calcGoldenCross)
import Sonowz.StockNoti.Stock.Types (StockSymbol)

runApp :: HasCallStack => Env -> IO Void
runApp env =
  (mainLoop (envStockSymbols env) >> threadDelay (fromIntegral (envWorkerIntervalSeconds env) * 10 ^ 6))
    & runStockDataSourceAlphaVantage
    & runReader (envPgConnection env)
    & runErrorAsLogging @ParseException
    & foreverCatch
    & timeToIO
    & embedToFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO

type MainLoopEffects = Final IO : StockDataSource : DBEffects

mainLoop :: (HasCallStack, Members MainLoopEffects r) => [StockSymbol] -> Sem r ()
mainLoop stocks = forM_ stocks $ \stock -> catchAnyException $ do
  logInfo $ "Checking " <> show stock <> "..."
  stockTimeSeries <- fetchDayStockPrices stock
  let smaPeriodShort = 5
      smaPeriodLong = 18
      deadCrossTimes = calcDeadCross smaPeriodShort smaPeriodLong stockTimeSeries
      goldenCrossTimes = calcGoldenCross smaPeriodShort smaPeriodLong stockTimeSeries
  case nonEmpty deadCrossTimes of
    Nothing -> logDebug $ show stock <> " has no dead cross."
    Just xs ->
      let lastDate = utctDay (last xs)
       in void $ createNotification stock NotiDeadCross lastDate
  case nonEmpty goldenCrossTimes of
    Nothing -> logDebug $ show stock <> " has no golden cross."
    Just xs ->
      let lastDate = utctDay (last xs)
       in void $ createNotification stock NotiGoldenCross lastDate
