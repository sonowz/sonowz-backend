{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}
module Sonowz.Noti.DB.Types where

import Data.Profunctor.Product.Default (Default(..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple.FromField as FF
import Opaleye
import Sonowz.Core.DB.Utils
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types (NotificationBody, NotificationType, Uid(..))



data Notification' c1 c2 c3 c4 c5 = Notification'
  { uid         :: c1
  , _type       :: c2
  , title       :: c3
  , body        :: c4
  , createdTime :: c5
  }

type NotificationHaskW = Notification' Void NotificationType Text NotificationBody Void
type NotificationHask = Notification' Uid NotificationType Text NotificationBody UTCTime
type NotificationFieldW
  = Notification' -- Write fields
      (Maybe (Field SqlInt4))
      (Field SqlText)
      (Field SqlText)
      (Field SqlText)
      (Maybe (Field SqlTimestamptz))
type NotificationFieldR
  = Notification' -- Read fields
      (Field SqlInt4)
      (Field SqlText)
      (Field SqlText)
      (Field SqlText)
      (Field SqlTimestamptz)
type NotificationTable = Table NotificationFieldW NotificationFieldR

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pNotification" ''Notification')
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
