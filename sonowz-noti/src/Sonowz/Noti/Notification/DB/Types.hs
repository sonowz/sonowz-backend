{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}
module Sonowz.Noti.Notification.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Core.DB.Utils (Uid)
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types (NotificationBody, NotificationType)


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

$(makeAdaptorAndInstance "pNotification" ''Notification')
