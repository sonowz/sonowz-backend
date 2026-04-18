module Sonowz.StockNoti.App
  ( runApp,
  )
where

import Data.Time (UTCTime (utctDay))
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Error.Effect (foreverCatch, runErrorAsLogging)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.Time.Effect (Time, threadDelay, timeToIOFinal)
import Sonowz.StockNoti.Env (Env (..))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Notification (StockNotificationType (NotiDeadCross, NotiGoldenCross), createNotification)
import Sonowz.StockNoti.Stock.DataSource.AlphaVantage (runStockDataSourceAlphaVantage)
import Sonowz.StockNoti.Stock.DataSource.Effect (StockDataSource, fetchDayStockPrices)
import Sonowz.StockNoti.Stock.Logic.Cross (calcDeadCross, calcGoldenCross)

runApp :: (HasCallStack) => Env -> IO Void
runApp env =
  mainLoop env
    & runStockDataSourceAlphaVantage
    & runReader (envPgConnection env)
    & runErrorAsLogging @ParseException
    & foreverCatch (sleep env)
    & embedToFinal
    & timeToIOFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO

sleep :: (Member Time r) => Env -> Sem r ()
sleep env = threadDelay (fromIntegral (envWorkerIntervalSeconds env) * 10 ^ 6)

type MainLoopEffects = [StockDataSource, Error ParseException, Time, StdLog] <> DBEffects

mainLoop :: (HasCallStack, Members MainLoopEffects r) => Env -> Sem r ()
mainLoop env = do
  let stocks = envStockSymbols env
  -- catch 'fetchDayStockPrices' errors
  forM_ stocks $ \stock -> runErrorAsLogging @ParseException $ do
    threadDelay (10 * 10 ^ 6) -- Sleep 10 seconds between stocks to avoid hitting API rate limits
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
  sleep env
