module Sonowz.Noti.App where

import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.MessageQueue.Effect.Void (runMQueueVoid)
import Sonowz.Core.MessageQueueThread.Effect
  (StreamHandler, StreamResult(HContinue), doStreamLoop, runMQueueStream)
import Sonowz.Core.Time.Effect (Time, threadDelay, timeToIO)
import Sonowz.Noti.DB.Queries (deleteNotificationByUid)
import Sonowz.Noti.Env (Env(..))
import Sonowz.Noti.Imports
import Sonowz.Noti.MessageQueue.DB (runMQueueDBNoti)
import Sonowz.Noti.Notification.Handler.Email (EmailConfig, generateEmailNotification)
import Sonowz.Noti.Notification.Types (Notification(..), NotificationType(..))


runApp :: Env -> IO ()
runApp Env {..} =
  forever (catch (fromExceptionSem doStreamLoop) handleError)
    & runMQueueStream notificationHandler
    & runMQueueDBNoti
    & runMQueueVoid
    & runReader envEmailConfig
    & runReader envPgConnection
    & timeToIO
    & embedToFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO
 where
  handleError :: Members '[Time , StdLog] r => SomeException -> Sem r ()
  handleError e = do
    logError "Critical error, sleeping for 1 minute... (details below)"
    logException e
    threadDelay (60 * 10 ^ 6)

type HandlerEffects =
  Reader EmailConfig
  : Embed IO
  : DBEffects

notificationHandler :: Members HandlerEffects r => StreamHandler r Notification Void
notificationHandler noti =
  HContinue
    <$ (case notificationType noti of
         Email -> do
           let Just uid = notificationUid noti
           logInfo $ "Generate email: '" <> notificationTitle noti <> "'"
           generateEmailNotification noti
           withDBConn (\conn -> liftIO $ deleteNotificationByUid conn uid)
       )
