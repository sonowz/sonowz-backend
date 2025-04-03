module Sonowz.NewsCombinator.App.RuleWorker
  ( runRuleWorker,
  )
where

import Network.HTTP.Client (HttpException)
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Error.Effect (foreverCatch, runErrorAsLogging, unsafeErrorToIO)
import Sonowz.Core.HTTP.Effect (HTTP, runHTTPIO)
import Sonowz.Core.Time.Effect (Time, threadDelay, timeToIO)
import Sonowz.NewsCombinator.Env (Env (..))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Notification (createNotification)
import Sonowz.NewsCombinator.News.Parser (ParseException)
import Sonowz.NewsCombinator.Rule.DB.Queries (getNewsScrapRules, updateNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Executor (evalNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))

runRuleWorker :: HasCallStack => Env -> IO Void
runRuleWorker env =
  foreverCatch sleep (worker >> sleep)
    & runHTTPIO
    & unsafeErrorToIO @HttpException
    & runReader (envPgConnection env)
    & timeToIO
    & embedToFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO
  where
    sleep = threadDelay (fromIntegral (envWorkerIntervalSeconds env) * 10 ^ 6)

type WorkerEffects = Final IO : Time : HTTP : DBEffects

worker :: HasCallStack => Members WorkerEffects r => Sem r ()
worker = do
  rules <- filter isEnabled <$> withDBConn (liftIO . getNewsScrapRules)
  mapM_
    ( \rule -> runErrorAsLogging @SomeException $ fromExceptionSem $ mapError @HttpException toException $ do
        logDebug ("Evaluate \"" <> keyword rule <> "\"...")
        (newsItems, rule') <- mapError @ParseException toException $ evalNewsScrapRule rule
        case newsItems of
          Just newsItems' -> logDebug "Success!" >> void (createNotification rule newsItems')
          Nothing -> pass
        withDBConn (\conn -> liftIO $ updateNewsScrapRule conn rule')
    )
    rules
