module Sonowz.Noti.App where

import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Error.Effect (foreverCatch)
import Sonowz.Core.MessageQueue.Effect.Void (runMQueueVoid)
import Sonowz.Core.MessageQueueThread.Effect
  ( StreamHandler,
    StreamResult (HContinue),
    doStreamLoop,
    runMQueueStream,
  )
import Sonowz.Core.Time.Effect (threadDelay, timeToIOFinal)
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports
import Sonowz.Noti.MessageQueue.DB (runMQueueDBNoti)
import Sonowz.Noti.Notification.DB.Queries (deleteNotificationByUid)
import Sonowz.Noti.Notification.Handler.Email (EmailConfig, generateEmailNotification)
import Sonowz.Noti.Notification.Types (Notification (..), NotificationType (..))

runApp :: (HasCallStack) => Env -> IO Void
runApp Env {..} =
  (doStreamLoop >> threadDelay (60 * 10 ^ 6))
    & foreverCatch (threadDelay (60 * 10 ^ 6))
    & runMQueueStream notificationHandler
    & runMQueueDBNoti
    & runMQueueVoid
    & runReader envEmailConfig
    & runReader envPgConnection
    & embedToFinal
    & timeToIOFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal @IO

type HandlerEffects = Reader EmailConfig : Embed IO : DBEffects

notificationHandler ::
  (Members HandlerEffects r, HasCallStack) => StreamHandler r Notification Void
notificationHandler noti =
  HContinue
    <$ ( case notificationType noti of
           Email -> do
             let Just uid = notificationUid noti
             logInfo $ "Generate email: '" <> notificationTitle noti <> "'"
             generateEmailNotification noti
             withDBConn (\conn -> liftIO $ deleteNotificationByUid conn uid)
       )
