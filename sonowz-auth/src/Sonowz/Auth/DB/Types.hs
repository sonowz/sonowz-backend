{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Auth.DB.Types where

import Opaleye
import Data.Time
import Data.Profunctor.Product.Default (Default(..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Sonowz.Auth.Imports


newtype DatabaseException = DatabaseException Text deriving (Show, Exception)

newtype Uid = Uid Int
  deriving (Eq, Show, Read) deriving (Num) via Int

-- 'uid' must be unique
-- ('oauthProvider', 'oauthId') must be unique
data User c1 c2 c3 c4 c5 = User
  { uid :: c1
  , oauthProvider :: c2
  , oauthId :: c3
  , representation :: c4
  , createdTime :: c5
  } deriving (Show, Read)
type UserInfo = User Uid Text Text Text UTCTime
type UserFieldW = User -- Write fields
  (Maybe (Field SqlInt4))
  (Field SqlText)
  (Field SqlText)
  (Field SqlText)
  (Maybe (Field SqlTimestamptz))
type UserFieldR = User -- Read fields
  (Field SqlInt4)
  (Field SqlText)
  (Field SqlText)
  (Field SqlText)
  (Field SqlTimestamptz)
type UserTable = Table UserFieldW UserFieldR


-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pUser" ''User)
deriving via Int instance QueryRunnerColumnDefault SqlInt4 Uid
instance Default Constant Uid (Column SqlInt4) where
  def = coerce (def :: Constant Int (Column SqlInt4))
