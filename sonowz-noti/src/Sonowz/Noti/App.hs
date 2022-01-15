module Sonowz.Noti.App where

import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.MessageQueue.Effect.Void (runMQueueVoid)
import Sonowz.Core.MessageQueueThread.Effect
  (StreamHandler, StreamResult(HContinue), doStreamLoop, runMQueueStream)
import Sonowz.Core.Time.Effect (timeToIO)
import Sonowz.Noti.Env (Env(..))
import Sonowz.Noti.Imports
import Sonowz.Noti.MessageQueue.DB (runMQueueDBNoti)
import Sonowz.Noti.Notification.Handler.Email (EmailConfig, generateEmailNotification)
import Sonowz.Noti.Notification.Types (Notification(..), NotificationType(..))


runApp :: Env -> IO ()
runApp Env {..} =
  forever (catch doStreamLoop logException)
    & runMQueueStream notificationHandler
    & runMQueueDBNoti
    & runMQueueVoid
    & runReader envEmailConfig
    & runReader envPgConnection
    & timeToIO
    & stdEffToIO
    & embedToFinal
    & resourceToIOFinal
    & runFinal @IO

type HandlerEffects =
  Reader EmailConfig
  : Embed IO
  : StdEff

notificationHandler :: Members HandlerEffects r => StreamHandler r Notification Void
notificationHandler noti =
  HContinue
    <$ (case notificationType noti of
         Email -> do
           logInfo $ "Generate email: '" <> notificationTitle noti <> "'"
           generateEmailNotification noti
       )
