module Sonowz.NewsCombinator.App.RuleWorker
  ( runRuleWorker,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe qualified as E
import Network.HTTP.Client (HttpException)
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.HTTP.Effect (HTTP, runHTTPIO)
import Sonowz.Core.Time.Effect (Time, timeToIO)
import Sonowz.NewsCombinator.Env (Env (..))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Notification (createNotification)
import Sonowz.NewsCombinator.News.Parser (ParseException)
import Sonowz.NewsCombinator.Rule.DB.Queries (getNewsScrapRules, updateNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Executor (evalNewsScrapRule)
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))

runRuleWorker :: HasCallStack => Env -> IO ()
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
      | otherwise = threadDelay (fromIntegral t)

    workerIO :: Env -> IO ()
    workerIO env =
      worker
        & runHTTPIO
        & runError' @HttpException
        & runReader (envPgConnection env)
        & timeToIO
        & embedToFinal
        & resourceToIOFinal
        & stdEffToIOFinal
        & runFinal @IO
      where
        runError' :: forall e r a. (Exception e, Member (Embed IO) r) => Sem (Error e : r) a -> Sem r a
        runError' = liftIO . either E.throw pure <=< runError

type WorkerEffects = Final IO : Time : HTTP : DBEffects

worker :: HasCallStack => Members WorkerEffects r => Sem r ()
worker = do
  rules <- filter isEnabled <$> withDBConn (liftIO . getNewsScrapRules)
  mapM_
    -- TODO: refactor this effect stack
    ( \rule -> runError @SomeException . flip catch logException . fromExceptionSem $ mapError @HttpException toException $ do
        -- Continues even when exception is thrown
        logDebug ("Evaluate \"" <> keyword rule <> "\"...")
        (newsItems, rule') <- mapError (toException @ParseException) $ evalNewsScrapRule rule
        case newsItems of
          Just newsItems' -> logDebug "Success!" >> void (createNotification rule newsItems')
          Nothing -> pass
        withDBConn (\conn -> liftIO $ updateNewsScrapRule conn rule')
    )
    rules
