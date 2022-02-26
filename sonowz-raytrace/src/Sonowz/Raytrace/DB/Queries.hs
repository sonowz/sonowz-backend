{-# LANGUAGE Arrows #-}
module Sonowz.Raytrace.DB.Queries where

import Control.Arrow
import Data.Profunctor (dimap)
import Data.Profunctor.Product (p3)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye
import qualified Opaleye.Aggregate as Agg

import Sonowz.Raytrace.Imports hiding (null)

import Sonowz.Raytrace.DB.Types

-- Message queue is implemented with PostgreSQL, for studying.
-- Queries are written in a way that they just look like plain code (not SQL),
-- so they are quite inefficient in terms of performance.
-- http://haskell.vacationlabs.com/en/latest/docs/opaleye/advanced-db-mapping.html
-- https://gitlab.com/williamyaoh/haskell-orm-comparison/-/blob/master/opaleye-impl/src/Lib.hs


-- Table declarations --

{-
CREATE TABLE public.daemon_message_queue (
    qid serial PRIMARY KEY NOT NULL,
    servant_id integer NOT NULL,
    operation text NOT NULL,
    created_time timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.servant_message_queue (
    qid serial PRIMARY KEY NOT NULL,
    servant_id integer NOT NULL,
    operation text NOT NULL,
    created_time timestamp with time zone DEFAULT now() NOT NULL
);
-}

daemonMessageQueue :: DaemonMessageTable
daemonMessageQueue = table "daemon_message_queue" (pMessage messageQueueFields)

servantMessageQueue :: ServantMessageTable
servantMessageQueue = table "servant_message_queue" (pMessage messageQueueFields)

type SeqTable
  = Table
      (Field SqlInt4, Field SqlInt4, Field SqlBool)
      (Field SqlInt4, Field SqlInt4, Field SqlBool)

seqTableTemplate :: String -> SeqTable
seqTableTemplate seqName = table seqName (p3 fields)
  where fields = (tableField "last_value", tableField "log_cnt", tableField "is_called")

daemonMessageSeq :: SeqTable
daemonMessageSeq = seqTableTemplate "daemon_message_queue_qid_seq"

servantMessageSeq :: SeqTable
servantMessageSeq = seqTableTemplate "servant_message_queue_qid_seq"

messageQueueFields = Message
  { qid         = tableField "qid"
  , servantId   = tableField "servant_id"
  , operation   = tableField "operation"
  , createdTime = tableField "created_time"
  }

emptyAggMessageQueue =
  Message { qid = nullify, servantId = nullify, operation = nullify, createdTime = nullify }


-- Public Interfaces --

enqueueDaemon :: HasCallStack => Connection -> ServantId -> DaemonOp -> IO Bool
enqueueDaemon conn servantId' message =
  logCheckBool =<< insertIsSuccess <$> runInsert_
    conn
    (insertMessage daemonMessageQueue servantId' message) :: IO Bool

-- This function sets 'servantId' same as 'qid'
enqueueDaemonNew :: HasCallStack => Connection -> DaemonOp -> IO (Maybe ServantId)
enqueueDaemonNew conn message = withTransaction conn $ do
  maxQid <- qidOrZero <$> selectResult conn
  let newServantId = ServantId (coerce maxQid)
  insertResult conn newServantId message >>= logCheckBool >>= \success ->
    if success then return (Just newServantId) else return Nothing
 where
  selectResult conn = Qid <<$>> runSelect conn (selectMaxSeq daemonMessageSeq) :: IO [Qid]
  insertResult conn sid msg =
    insertIsSuccess <$> runInsert_ conn (insertMessage daemonMessageQueue sid msg) :: IO Bool

enqueueServant :: HasCallStack => Connection -> ServantId -> ServantOp -> IO Bool
enqueueServant conn servantId' message =
  logCheckBool =<< insertIsSuccess <$> runInsert_
    conn
    (insertMessage servantMessageQueue servantId' message) :: IO Bool

dequeueDaemon :: HasCallStack => Connection -> IO (Maybe DaemonMessage)
dequeueDaemon conn = withTransaction conn $ runMaybeT $ do
  targetQid <- MaybeT $ listToMaybe <$> selectResult conn
  MaybeT $ listToMaybe <$> deleteResult conn targetQid >>= logCheckMaybe where
  selectResult conn = Qid <<$>> runSelect conn (selectMinQid daemonMessageQueue) :: IO [Qid]
  deleteResult conn qid = runDelete_ conn (popMessage daemonMessageQueue qid) :: IO [DaemonMessage]

dequeueServant :: HasCallStack => Connection -> ServantId -> IO (Maybe ServantMessage)
dequeueServant conn sid = withTransaction conn $ runMaybeT $ do
  targetQid <- MaybeT $ listToMaybe <$> selectResult conn
  MaybeT $ listToMaybe <$> deleteResult conn targetQid >>= logCheckMaybe
 where
  selectResult conn =
    Qid <<$>> runSelect conn (selectServantMinQid servantMessageQueue `putArg` sid) :: IO [Qid]
  deleteResult conn qid =
    runDelete_ conn (popMessage servantMessageQueue qid) :: IO [ServantMessage]

-- Queries --

selectMinQid :: MessageTable op -> Select (Field SqlInt4)
selectMinQid table =
  qid <$> Agg.aggregate (pMessage $ emptyAggMessageQueue { qid = Agg.min }) (selectTable table)

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
    (pMessage $ emptyAggMessageQueue { qid = Agg.min, servantId = Agg.groupBy })
    (selectTable table)
    -< ()
  restrict -< servantId row .== toFields servantIdEq
  returnA -< qid row

popMessage
  :: QueryRunnerColumnDefault SqlText op => MessageTable op -> Qid -> Delete [MessageHask op]
popMessage dTable qidEq = Delete { .. } where
  dWhere (qid -> rowQid) = rowQid .== toFields qidEq
  dReturning = rReturning id

insertMessage
  :: Default Constant op (Column SqlText) => MessageTable op -> ServantId -> op -> Insert Int64
insertMessage table servantId' operation' = Insert
  { iTable      = table
  , iRows       = [message]
  , iReturning  = rCount
  , iOnConflict = Just DoNothing
  } where
  message = Message
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

logWarnDB :: HasCallStack => IO ()
logWarnDB = withFrozenCallStack $ logWarningIO "Query result might be wrong!"

logCheckBool :: HasCallStack => Bool -> IO Bool
logCheckBool False = withFrozenCallStack $ logWarnDB >> return False
logCheckBool True  = withFrozenCallStack $ return True

logCheckMaybe :: HasCallStack => Maybe a -> IO (Maybe a)
logCheckMaybe Nothing = withFrozenCallStack $ logWarnDB >> return Nothing
logCheckMaybe x       = withFrozenCallStack $ return x
