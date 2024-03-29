module Sonowz.Noti.Notification.Types
  ( Notification (..),
    NotificationType (..),
    NotificationBody (..),
  )
where

import Data.Profunctor.Product.Default (Default (..))
import Database.PostgreSQL.Simple.FromField qualified as FF
import Opaleye
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Utils (fromFieldSimple)
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

instance DefaultFromField SqlText NotificationType where
  defaultFromField = fromPGSFromField

instance DefaultFromField SqlText NotificationBody where
  defaultFromField = fromPGSFromField

instance Default ToFields NotificationType (Field SqlText) where
  def = toToFields (sqlStrictText . show)

instance Default ToFields NotificationBody (Field SqlText) where
  def = toToFields (sqlStrictText . show)

instance FF.FromField NotificationType where
  fromField = fromFieldSimple (readEither . decodeUtf8)

instance FF.FromField NotificationBody where
  fromField = fromFieldSimple (readEither . decodeUtf8)
