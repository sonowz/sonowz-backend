{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.Noti.Notification.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.Noti.Imports

data Notification' c1 c2 c3 c4 c5 = Notification'
  { uid :: c1,
    _type :: c2,
    title :: c3,
    body :: c4,
    createdTime :: c5
  }

type NotificationWriteDto = Notification' EmptyField Text Text Text EmptyField

type NotificationDto = Notification' Uid String Text String UTCTime

type NotificationFieldW =
  Notification' -- Write fields
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Maybe (Field SqlTimestamptz))

type NotificationFieldR =
  Notification' -- Read fields
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlTimestamptz)

instance Entity NotificationFieldR where
  entityIdField = uid
  entityToFields _ = toFields

type NotificationTable = Table NotificationFieldW NotificationFieldR

$(makeAdaptorAndInstance "pNotification" ''Notification')
