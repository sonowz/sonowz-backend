{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.Auth.OAuth.DB.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Servant.Auth.Server (FromJWT, ToJWT)
import Sonowz.Auth.Imports
import Sonowz.Core.DB.Utils (Uid)

-- 'uid' must be unique
-- ('oauthProvider', 'oauthId') must be unique
data User c1 c2 c3 c4 c5 = User
  { uid :: c1,
    oauthProvider :: c2,
    oauthId :: c3,
    representation :: c4,
    createdTime :: c5
  }
  deriving (Show, Read, Generic)

type UserInfoW = User Void Text Text Text Void

type UserInfo = User Uid Text Text Text UTCTime

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

type UserTable = Table UserFieldW UserFieldR

instance ToJSON UserInfo

instance ToJWT UserInfo

instance FromJSON UserInfo

instance FromJWT UserInfo

$(makeAdaptorAndInstance "pUser" ''User)
