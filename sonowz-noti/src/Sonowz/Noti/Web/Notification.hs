module Sonowz.Noti.Web.Notification
  ( NotificationAPI,
    notificationHandler,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Servant
import Sonowz.Core.MessageQueue.Effect (MessageQueue, enqueue)
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types (Notification (..), NotificationBody (..), NotificationType (..))

type NotificationAPI = "notifications" :> "email" :> ReqBody '[JSON] NotificationRequest :> Post '[JSON] ()

data NotificationRequest = NotificationRequest
  { title :: Text,
    body :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

notificationHandler :: (Members '[MessageQueue Notification] r) => ServerT NotificationAPI (Sem r)
notificationHandler NotificationRequest {..} = do
  let noti = Notification Email title (TextBody body) Nothing
  enqueue noti
