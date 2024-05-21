module Sonowz.Noti.Notification.Types
  ( Notification (..),
    NotificationType (..),
    NotificationBody (..),
  )
where

import Sonowz.Core.DB.Field (Uid)
import Sonowz.Noti.Imports

data NotificationType = Email
  deriving (Read, Show)

data NotificationBody = HTMLBody Text | TextBody Text
  deriving (Read, Show)

data Notification = Notification
  { notificationType :: NotificationType,
    notificationTitle :: Text,
    notificationBody :: NotificationBody,
    notificationUid :: Maybe Uid -- Unique ID used in DB
  }

instance Semigroup NotificationBody where
  HTMLBody t1 <> HTMLBody t2 = HTMLBody (t1 <> t2)
  HTMLBody t1 <> TextBody t2 = HTMLBody (t1 <> t2)
  TextBody t1 <> HTMLBody t2 = HTMLBody (t1 <> t2)
  TextBody t1 <> TextBody t2 = TextBody (t1 <> t2)

instance Monoid NotificationBody where
  mempty = TextBody ""
