module Sonowz.Noti.Notification.Types
  ( Notification(..)
  , NotificationType(..)
  , NotificationBody(..)
  , Uid(..)
  ) where

import Sonowz.Noti.Imports

newtype Uid = Uid Int
  deriving (Eq, Show, Read)
data NotificationType = Email
  deriving (Read, Show)
data NotificationBody = HTMLBody Text | TextBody Text
  deriving (Read, Show)

data Notification = Notification
  { notificationType  :: NotificationType
  , notificationTitle :: Text
  , notificationBody  :: NotificationBody
  , notificationUid   :: Maybe Uid -- Unique ID used in DB
  }
