{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Raytrace.DB.Types where

import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time
import Database.PostgreSQL.Simple.FromField qualified as FF
import Opaleye
import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.RaytraceConfig (Config (..))

newtype DatabaseException = DatabaseException Text
  deriving (Show)
  deriving anyclass (Exception)

newtype Qid = Qid Int
  deriving (Eq, Show, Read)
  deriving (Num) via Int

newtype ServantId = ServantId Int
  deriving (Eq, Show, Read)

data DaemonOp
  = Enqueue Config
  | Dequeue
  deriving (Eq, Show, Read)

data ServantOp
  = Enqueued
  | Dequeued
  | RemainingQueue Int
  | ProcessStarted
  | ProcessFinished
  | ProcessFailed
  deriving (Eq, Show, Read)

data Message c1 c2 c3 c4 = Message
  { qid :: ~c1,
    servantId :: ~c2,
    operation :: ~c3,
    createdTime :: ~c4
  }
  deriving (Show, Read)

type MessageHask op = Message Qid ServantId op UTCTime

type MessageFieldW op =
  Message -- Write fields
    (Maybe (Field SqlInt4))
    (Field SqlInt4)
    (Field SqlText)
    (Maybe (Field SqlTimestamptz))

type MessageFieldR op =
  Message -- Read fields
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlTimestamptz)

type MessageTable op = Table (MessageFieldW op) (MessageFieldR op)

type DaemonMessageTable = MessageTable DaemonOp

type DaemonMessage = MessageHask DaemonOp

type ServantMessageTable = MessageTable ServantOp

type ServantMessage = MessageHask ServantOp

emptyMessage :: Message c1 c2 c3 c4
emptyMessage =
  Message
    { qid = error "Unexpected 'qid' access",
      servantId = error "Unexpected 'servantId' access",
      operation = error "Unexpected 'operation' access",
      createdTime = error "Unexpected 'createdTime' access"
    }

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pMessage" ''Message)

deriving via Int instance DefaultFromField SqlInt4 Qid

deriving via Int instance DefaultFromField SqlInt4 ServantId

instance DefaultFromField SqlText DaemonOp where
  defaultFromField = fromPGSFromField

instance DefaultFromField SqlText ServantOp where
  defaultFromField = fromPGSFromField

instance Default ToFields Qid (Field SqlInt4) where
  def = coerce (def :: ToFields Int (Column SqlInt4))

instance Default ToFields ServantId (Field SqlInt4) where
  def = coerce (def :: ToFields Int (Column SqlInt4))

instance Default ToFields DaemonOp (Field SqlText) where
  def = toToFields (sqlStrictText . show)

instance Default ToFields ServantOp (Field SqlText) where
  def = toToFields (sqlStrictText . show)

instance FF.FromField DaemonOp where
  fromField = ffByReadInstance "DaemonOp"

instance FF.FromField ServantOp where
  fromField = ffByReadInstance "ServantOp"

ffByReadInstance :: (Typeable a, Read a) => String -> FF.Field -> Maybe ByteString -> FF.Conversion a
ffByReadInstance name field (Just bs) =
  either
    (const $ FF.returnError FF.ConversionFailed field ("Parse failed in: " <> name))
    return
    (readEither . toString $ decodeUtf8 @Text bs)
ffByReadInstance name field Nothing = FF.returnError FF.UnexpectedNull field ("Null value in: " <> name)
