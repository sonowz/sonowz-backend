module Sonowz.NewsCombinator.App.RuleWorker
  ( runRuleWorker
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception.Safe as E
import Network.HTTP.Client (HttpException)
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Time.Effect (Time, timeToIO)
import Sonowz.NewsCombinator.Env (Env(..))
import Sonowz.NewsCombinator.HTTP.Effect (HTTP, runHTTPIO)
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Notification (createNotification)
import Sonowz.NewsCombinator.News.Parser (ParseException)
import Sonowz.NewsCombinator.Rule.DB.Queries (getNewsScrapRules, updateNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Executor (evalNewsScrapRule)


runRuleWorker :: Env -> IO ()
runRuleWorker env = forever $ do
  E.catchAny (workerIO env) printException
  threadDelay' (fromIntegral (envWorkerIntervalSeconds env) * 10 ^ 6)
 where
  printException :: SomeException -> IO ()
  printException e = logErrorIO "Unhandled exception:" >> logExceptionIO e

  -- 'threadDelay' with Integer parameter
  threadDelay' :: Integer -> IO ()
  threadDelay' t
    | t > 10 ^ 9 = threadDelay (10 ^ 9) >> threadDelay' (t - 10 ^ 9)
    | otherwise  = threadDelay (fromIntegral t)

  workerIO :: Env -> IO ()
  workerIO env =
    worker
      & runHTTPIO
      & mapError (toException @HttpException)
      & runReader (envPgConnection env)
      & timeToIO
      & embedToFinal
      & resourceToIOFinal
      & stdEffToIOFinal
      & runFinal @IO


type WorkerEffects = Final IO : Time : HTTP : DBEffects

worker :: Members WorkerEffects r => Sem r ()
worker = withDBConn (embed . getNewsScrapRules) >>= mapM_
  (\rule -> try $ do -- Continues even when exception is thrown
    (newsItems, rule') <- mapError (toException @ParseException) $ evalNewsScrapRule rule
    whenJust newsItems (void . createNotification rule)
    withDBConn (\conn -> fromException $ updateNewsScrapRule conn rule')
  )