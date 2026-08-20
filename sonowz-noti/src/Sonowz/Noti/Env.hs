module Sonowz.Noti.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.Noti.Notification.Handler.Email (EmailConfig)

data Env = Env
  { emailConfig :: EmailConfig,
    pgConnection :: DBConnPool
  }
