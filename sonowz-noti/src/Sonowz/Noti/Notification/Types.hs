module Sonowz.Noti.Notification.Types
  ( Notification(..)
  , NotificationType(..)
  , NotificationBody(..)
  ) where

import Data.Profunctor.Product.Default (Default(..))
import qualified Database.PostgreSQL.Simple.FromField as FF
import Opaleye
import Sonowz.Core.DB.Utils (Uid, fromFieldSimple)
import Sonowz.Noti.Imports

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

instance QueryRunnerColumnDefault SqlText NotificationType where
  defaultFromField = fieldQueryRunnerColumn
instance QueryRunnerColumnDefault SqlText NotificationBody where
  defaultFromField = fieldQueryRunnerColumn
instance Default Constant NotificationType (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance Default Constant NotificationBody (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance FF.FromField NotificationType where
  fromField = fromFieldSimple (readEither . decodeUtf8)
instance FF.FromField NotificationBody where
  fromField = fromFieldSimple (readEither . decodeUtf8)
