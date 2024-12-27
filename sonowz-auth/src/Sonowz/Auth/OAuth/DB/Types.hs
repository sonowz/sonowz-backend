{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.Auth.OAuth.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Auth.Imports
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)

-- 'uid' must be unique
-- ('oauthProvider', 'oauthId') must be unique
data User c1 c2 c3 c4 c5 = User
  { uid :: c1,
    oauthProvider :: c2,
    oauthId :: c3,
    representation :: c4,
    createdTime :: c5
  }
  deriving (Show, Read)

type UserInfoWriteDto = User EmptyField Text Text Text EmptyField

type UserInfoDto = User Uid Text Text Text UTCTime

type UserFieldW =
  User -- Write fields
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Maybe (Field SqlTimestamptz))

type UserFieldR =
  User -- Read fields
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlTimestamptz)

instance Entity UserFieldR where
  entityIdField = uid
  entityToFields _ = toFields

type UserTable = Table UserFieldW UserFieldR

$(makeAdaptorAndInstance "pUser" ''User)
