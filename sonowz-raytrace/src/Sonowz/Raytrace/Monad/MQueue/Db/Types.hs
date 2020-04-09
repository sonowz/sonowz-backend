{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.Monad.MQueue.Db.Types where

import Relude
import Opaleye
import Control.Concurrent (threadDelay)
import Data.Time
import Data.Profunctor.Product.Default (Default(..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple.FromField as FF

import Sonowz.Raytrace.RaytraceConfig (Config(..))

newtype Qid = Qid Int
  deriving (Eq, Show, Read) deriving (Num) via Int
newtype ServantId = ServantId Int
  deriving (Eq, Show, Read)

data DaemonOp =
    Enqueue Config
  | Dequeue
  deriving (Eq, Show, Read)

data ServantOp =
    Enqueued
  | Dequeued
  | RemainingQueue Int
  | ProcessStarted
  | ProcessFinished
  | ProcessFailed
  deriving (Eq, Show, Read)

data MessageQueue c1 c2 c3 c4 = MessageQueue {
  qid :: c1,
  servantId :: c2,
  operation :: c3,
  createdTime :: c4
} deriving (Show, Read)
type Message op = MessageQueue Qid ServantId op UTCTime
type MessageFieldW op = MessageQueue -- Write fields
  (Maybe (Field SqlInt4))
  (Field SqlInt4)
  (Field SqlText)
  (Maybe (Field SqlTimestamptz))
type MessageFieldR op = MessageQueue -- Read fields
  (Field SqlInt4)
  (Field SqlInt4)
  (Field SqlText)
  (Field SqlTimestamptz)
type MessageTable op = Table (MessageFieldW op) (MessageFieldR op)
type DaemonMessageTable = MessageTable DaemonOp
type DaemonMessage = Message DaemonOp
type ServantMessageTable = MessageTable ServantOp
type ServantMessage = Message ServantOp

emptyMessage :: MessageQueue c1 c2 c3 c4
emptyMessage = MessageQueue
  { qid = error "Unexpected 'qid' access"
  , servantId = error "Unexpected 'servantId' access"
  , operation = error "Unexpected 'operation' access"
  , createdTime = error "Unexpected 'createdTime' access"
  }

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pMessageQueue" ''MessageQueue)
deriving via Int instance QueryRunnerColumnDefault SqlInt4 Qid
deriving via Int instance QueryRunnerColumnDefault SqlInt4 ServantId
instance QueryRunnerColumnDefault SqlText DaemonOp where
  defaultFromField = fieldQueryRunnerColumn
instance QueryRunnerColumnDefault SqlText ServantOp where
  defaultFromField = fieldQueryRunnerColumn
instance Default Constant Qid (Column SqlInt4) where
  def = coerce (def :: Constant Int (Column SqlInt4))
instance Default Constant ServantId (Column SqlInt4) where
  def = coerce (def :: Constant Int (Column SqlInt4))
instance Default Constant DaemonOp (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance Default Constant ServantOp (Column SqlText) where
  def = Constant (sqlStrictText . show)
instance FF.FromField DaemonOp where
  fromField = ffByReadInstance "DaemonOp"
instance FF.FromField ServantOp where
  fromField = ffByReadInstance "ServantOp"
ffByReadInstance :: (Typeable a, Read a) => String -> FF.Field -> Maybe ByteString -> FF.Conversion a
ffByReadInstance name field (Just bs) = either
    (const $ FF.returnError FF.ConversionFailed field ("Parse failed in: " <> name))
    return
    (readEither $ decodeUtf8 @Text bs)
ffByReadInstance name field Nothing = FF.returnError FF.UnexpectedNull field ("Null value in: " <> name)

-- Thread interface --
-- Minimal definition: `peekMessage`
class MonadIO io => BlockingThread io msg where
  peekMessage :: io (Maybe msg)
  runBlockingThread :: (msg -> io ThreadHandleResult) -> io ()
  runBlockingThread handler = do
    handlerResult <- handler =<< getMessageBlocking
    case handlerResult of
      TContinue -> runBlockingThread handler
      TTerminate -> pass
  getMessageBlocking :: io msg
  getMessageBlocking = peekMessage >>= \case
    Just message -> return message
    Nothing -> liftIO (threadDelay 1000) >> getMessageBlocking

data ThreadHandleResult = TContinue | TTerminate
