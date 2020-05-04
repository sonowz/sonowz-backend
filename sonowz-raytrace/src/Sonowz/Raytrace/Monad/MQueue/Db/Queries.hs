{-# LANGUAGE Arrows #-}
module Sonowz.Raytrace.Monad.MQueue.Db.Queries where

import Relude hiding (null)
import Opaleye
import Control.Arrow
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Data.Profunctor (dimap)
import Data.Profunctor.Product (p3)
import qualified Opaleye.Aggregate as Agg

import Sonowz.Raytrace.Core.DB (DBConnPool, withDBConn)
import Sonowz.Raytrace.Monad.MQueue.Db.Types

-- Message queue is implemented with PostgreSQL, for studying.
-- Queries are written in a way that they just look like plain code (not SQL),
-- so they are quite inefficient in terms of performance.
-- http://haskell.vacationlabs.com/en/latest/docs/opaleye/advanced-db-mapping.html
-- https://gitlab.com/williamyaoh/haskell-orm-comparison/-/blob/master/opaleye-impl/src/Lib.hs


-- Table declarations --

daemonMessageQueue :: DaemonMessageTable
daemonMessageQueue = table "daemon_message_queue" (pMessageQueue messageQueueFields)

servantMessageQueue :: ServantMessageTable
servantMessageQueue = table "servant_message_queue" (pMessageQueue messageQueueFields)

type SeqTable = Table (Field SqlInt4, Field SqlInt4, Field SqlBool) (Field SqlInt4, Field SqlInt4, Field SqlBool)

seqTableTemplate :: String -> SeqTable
seqTableTemplate seqName = table seqName (p3 fields) where
  fields = (tableField "last_value"
          , tableField "log_cnt"
          , tableField "is_called")

daemonMessageSeq :: SeqTable
daemonMessageSeq = seqTableTemplate "daemon_message_queue_qid_seq"

servantMessageSeq :: SeqTable
servantMessageSeq = seqTableTemplate "servant_message_queue_qid_seq"

messageQueueFields = MessageQueue
  { qid         = tableField "qid"
  , servantId   = tableField "servant_id"
  , operation   = tableField "operation"
  , createdTime = tableField "created_time"
  }

emptyAggMessageQueue = MessageQueue
  { qid = nullify
  , servantId = nullify
  , operation = nullify
  , createdTime = nullify }


-- Public Interfaces --

enqueueDaemon :: DBConnPool -> ServantId -> DaemonOp -> IO Bool
enqueueDaemon pool servantId' message = withDBConn pool $ \conn ->
  insertIsSuccess <$> runInsert_ conn (insertMessage daemonMessageQueue servantId' message) :: IO Bool

-- This function sets 'servantId' same as 'qid'
enqueueDaemonNew :: DBConnPool -> DaemonOp -> IO (Maybe ServantId)
enqueueDaemonNew pool message = withDBConn pool $ \conn -> withTransaction conn $ do
  maxQid <- qidOrZero <$> selectResult conn
  let newServantId = ServantId (coerce maxQid)
  insertResult conn newServantId message >>= \success -> if success
    then return (Just newServantId)
    else return Nothing
  where
    selectResult conn = Qid <<$>> runSelect conn (selectMaxSeq daemonMessageSeq) :: IO [Qid]
    insertResult conn sid msg = insertIsSuccess <$> runInsert_ conn (insertMessage daemonMessageQueue sid msg) :: IO Bool

enqueueServant :: DBConnPool -> ServantId -> ServantOp -> IO Bool
enqueueServant pool servantId' message = withDBConn pool $ \conn ->
  insertIsSuccess <$> runInsert_ conn (insertMessage servantMessageQueue servantId' message) :: IO Bool

dequeueDaemon :: DBConnPool -> IO (Maybe DaemonMessage)
dequeueDaemon pool = withDBConn pool $ \conn ->
  withTransaction conn $ runMaybeT $ do
    targetQid <- MaybeT $ listToMaybe <$> selectResult conn
    MaybeT $ listToMaybe <$> deleteResult conn targetQid where
      selectResult conn = Qid <<$>> runSelect conn (selectMinQid daemonMessageQueue) :: IO [Qid]
      deleteResult conn qid = runDelete_ conn (popMessage daemonMessageQueue qid) :: IO [DaemonMessage]

dequeueServant :: DBConnPool -> ServantId -> IO (Maybe ServantMessage)
dequeueServant pool sid = withDBConn pool $ \conn -> 
  withTransaction conn $ runMaybeT $ do
    targetQid <- MaybeT $ listToMaybe <$> selectResult conn
    MaybeT $ listToMaybe <$> deleteResult conn targetQid
    where
      selectResult conn = Qid <<$>> runSelect conn (selectServantMinQid servantMessageQueue `putArg` sid) :: IO [Qid]
      deleteResult conn qid = runDelete_ conn (popMessage servantMessageQueue qid) :: IO [ServantMessage]

-- Queries --

selectMinQid :: MessageTable op -> Select (Field SqlInt4)
selectMinQid table = qid <$> Agg.aggregate
  (pMessageQueue $ emptyAggMessageQueue { qid = Agg.min })
  (selectTable table)

-- You may not want to use this! Use 'selectMaxSeq' instead.
{- 
selectMaxQid :: MessageTable op -> Select (Field SqlInt4)
selectMaxQid table = qid <$> Agg.aggregate
  (pMessageQueue $ emptyAggMessageQueue { qid = Agg.max })
  (selectTable table)
 -}

selectMaxSeq :: SeqTable -> Select (Field SqlInt4)
selectMaxSeq seqTable = proc () -> do
  (value, _, _) <- selectTable seqTable -< ()
  returnA -< value

selectServantMinQid :: MessageTable op -> SelectArr ServantId (Field SqlInt4)
selectServantMinQid table = proc servantIdEq -> do
  row <- Agg.aggregate
    (pMessageQueue $ emptyAggMessageQueue { qid = Agg.min, servantId = Agg.groupBy })
    (selectTable table)
    -< ()
  restrict -< servantId row .== toFields servantIdEq
  returnA -< qid row

popMessage :: QueryRunnerColumnDefault SqlText op =>
  MessageTable op -> Qid -> Delete [Message op]
popMessage dTable qidEq = Delete{..} where
  dWhere (qid -> rowQid) = rowQid .== toFields qidEq
  dReturning = rReturning id

insertMessage :: Default Constant op (Column SqlText) =>
  MessageTable op -> ServantId -> op -> Insert Int64
insertMessage table servantId' operation' = Insert
  { iTable = table
  , iRows = [message]
  , iReturning = rCount
  , iOnConflict = Just DoNothing
  } where
    message = MessageQueue
      { qid         = Nothing
      , servantId   = toFields servantId'
      , operation   = toFields operation'
      , createdTime = Nothing
      }

-- Opaleye helper functions --

nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f

qidOrZero :: [Qid] -> Qid
qidOrZero = fromMaybe 0 . listToMaybe

insertIsSuccess :: Int64 -> Bool
insertIsSuccess result = result > 0
