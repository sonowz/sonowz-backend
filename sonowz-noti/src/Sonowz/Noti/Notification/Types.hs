module Sonowz.Noti.Notification.Types
  ( Notification(..)
  , NotificationType(..)
  , NotificationBody(..)
  , Uid(..)
  ) where

import Data.Profunctor.Product.Default (Default(..))
import qualified Database.PostgreSQL.Simple.FromField as FF
import Opaleye
import Sonowz.Core.DB.Utils (fromFieldSimple)
import Sonowz.Noti.Imports

newtype Uid = Uid Int
  deriving (Eq, Show, Read, Generic)
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

deriving via Int instance QueryRunnerColumnDefault SqlInt4 Uid
instance QueryRunnerColumnDefault SqlText NotificationType where
  defaultFromField = fieldQueryRunnerColumn
instance QueryRunnerColumnDefault SqlText NotificationBody where
  defaultFromField = fieldQueryRunnerColumn
instance Default Constant Uid (Column SqlInt4) where
  def = coerce (def :: Constant Int (Column SqlInt4))
instance Default Constant NotificationType (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance Default Constant NotificationBody (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance FF.FromField NotificationType where
  fromField = fromFieldSimple (readEither . decodeUtf8)
instance FF.FromField NotificationBody where
  fromField = fromFieldSimple (readEither . decodeUtf8)
