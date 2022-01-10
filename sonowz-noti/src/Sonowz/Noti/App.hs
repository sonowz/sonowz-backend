module Sonowz.Noti.App where

import Sonowz.Noti.Env (Env(..))
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Handler.Email (EmailConfig, generateEmailNotification)
import Sonowz.Noti.Notification.Types (Notification(..), NotificationType(..))

runApp :: Env -> IO ()
runApp Env {..} = undefined

appMain :: Members HandlerEffects r => Sem r ()
appMain = forever $ flip catch logException $ do
  noti <- fetchNotification
  handleNotification noti

fetchNotification :: Sem r Notification
fetchNotification = undefined

type HandlerEffects =
  Reader EmailConfig
  : Embed IO
  : StdEff

handleNotification :: Members HandlerEffects r => Notification -> Sem r ()
handleNotification noti = case notificationType noti of
  Email -> do
    logInfo $ "Generate email: '" <> notificationTitle noti <> "'"
    generateEmailNotification noti
