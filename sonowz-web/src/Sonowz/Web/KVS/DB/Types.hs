{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.Web.KVS.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Web.Imports

-- 'uid' must be unique
-- ('oauthId', 'key') must be unique
data KVS c1 c2 c3 c4 c5 c6 = KVS
  { uid :: c1,
    oauthId :: c2,
    key :: c3,
    value :: c4,
    createdTime :: c5,
    updatedTime :: c6
  }
  deriving (Show, Read, Generic)

type KVSHaskW = KVS Void Text Text Text Void Void

type KVSHaskR = KVS Uid Text Text Text UTCTime UTCTime

type KVSW =
  KVS
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Maybe (Field SqlTimestamptz))
    (Maybe (Field SqlTimestamptz))

type KVSR =
  KVS
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)

type KVSTable = Table KVSW KVSR

$(makeAdaptorAndInstance "pKVS" ''KVS)
