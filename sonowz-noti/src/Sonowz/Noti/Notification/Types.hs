module Sonowz.Noti.Notification.Types
  ( Notification(..)
  , NotificationType(..)
  , NotificationBody(..)
  ) where

import Sonowz.Noti.Imports


data NotificationType = Email
  deriving (Read, Show)
data NotificationBody = HTMLBody Text | TextBody Text
  deriving (Read, Show)

data Notification = Notification
  { notificationType  :: NotificationType
  , notificationTitle :: Text
  , notificationBody  :: NotificationBody
  }
