module Sonowz.NewsCombinator.App.RuleWorker
  ( runRuleWorker,
  )
where

import Network.HTTP.Client (HttpException)
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Error.Effect (foreverCatch, runErrorAsLogging, unsafeErrorToIO)
import Sonowz.Core.Http.Effect (runHttpIO)
import Sonowz.Core.Llm.Effect (Llm, LlmException, runHttpGemini)
import Sonowz.Core.Time.Effect (Time, threadDelay, timeToIOFinal)
import Sonowz.NewsCombinator.Env (Env (..))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Notification (createNotification)
import Sonowz.NewsCombinator.Rule.DB.Queries (getNewsScrapRules, updateNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Executor (evalNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))

runRuleWorker :: (HasCallStack) => Env -> IO Void
runRuleWorker env =
  foreverCatch sleep (worker >> sleep)
    & runHttpGemini
    & runHttpIO
    & unsafeErrorToIO @LlmException
    & unsafeErrorToIO @HttpException
    & runReader (env.llmEnv)
    & runReader (env.pgConnection)
    & embedToFinal
    & timeToIOFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO
  where
    sleep = threadDelay (fromIntegral env.workerIntervalSeconds * 10 ^ 6)

type WorkerEffects = Llm : Error LlmException : Final IO : Time : DBEffects

worker :: (HasCallStack) => (Members WorkerEffects r) => Sem r ()
worker = do
  rules <- filter (.isEnabled) <$> withDBConn (liftIO . getNewsScrapRules)
  mapM_
    ( \rule -> runErrorAsLogging @SomeException $ fromExceptionSem $ mapError @LlmException toException $ do
        logDebug ("Evaluate \"" <> rule.description <> "\"...")
        (evalResult, rule') <- evalNewsScrapRule rule
        case evalResult of
          Just res -> logDebug "Match found!" >> void (createNotification rule res)
          Nothing -> pass
        withDBConn (\conn -> liftIO $ updateNewsScrapRule conn rule')
    )
    rules
