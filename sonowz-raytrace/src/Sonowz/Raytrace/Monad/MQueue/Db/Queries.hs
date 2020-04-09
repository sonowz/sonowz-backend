{-# LANGUAGE Arrows #-}
module Sonowz.Raytrace.Monad.MQueue.Db.Queries where

import Relude hiding (null)
import Relude.Extra.Newtype
import Opaleye
import Control.Arrow
import Data.Time
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Data.Profunctor (dimap)
import UnliftIO.Exception
import qualified Opaleye.Aggregate as Agg

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

type DBConnection = Connection

enqueueDaemon :: DBConnection -> ServantId -> DaemonOp -> IO Bool
enqueueDaemon conn servantId' message =
  insertIsSuccess <$> runInsert_ conn (insertMessage daemonMessageQueue servantId' message) :: IO Bool

-- This function sets 'servantId' same as 'qid'
enqueueDaemonNew :: DBConnection -> DaemonOp -> IO (Maybe ServantId)
enqueueDaemonNew conn message = withTransaction conn $ do
  prevQid <- qidOrZero <$> selectResult
  let newServantId = ServantId (un prevQid + 1)
  insertResult newServantId message >>= \success -> if success
    then return (Just newServantId)
    else return Nothing
  where
    selectResult = Qid <<$>> runSelect conn (selectMaxQid daemonMessageQueue) :: IO [Qid]
    insertResult sid msg = insertIsSuccess <$> runInsert_ conn (insertMessage daemonMessageQueue sid msg) :: IO Bool

enqueueServant :: DBConnection -> ServantId -> DaemonOp -> IO Bool
enqueueServant conn servantId' message =
  insertIsSuccess <$> runInsert_ conn (insertMessage servantMessageQueue servantId' message) :: IO Bool

dequeueDaemon :: DBConnection -> IO (Maybe DaemonMessage)
dequeueDaemon conn = withTransaction conn $ runMaybeT $ do
  targetQid <- MaybeT $ listToMaybe <$> selectResult
  MaybeT $ listToMaybe <$> deleteResult targetQid where
    selectResult = Qid <<$>> runSelect conn (selectMinQid daemonMessageQueue) :: IO [Qid]
    deleteResult qid = runDelete_ conn (popMessage daemonMessageQueue qid) :: IO [DaemonMessage]

dequeueServant :: DBConnection -> ServantId -> IO (Maybe ServantMessage)
dequeueServant conn sid = withTransaction conn $ runMaybeT $ do
  targetQid <- MaybeT $ listToMaybe <$> selectResult
  MaybeT $ listToMaybe <$> deleteResult targetQid
  where
    selectResult = Qid <<$>> runSelect conn (selectServantMinQid servantMessageQueue `putArg` sid) :: IO [Qid]
    deleteResult qid = runDelete_ conn (popMessage servantMessageQueue qid) :: IO [ServantMessage]

getStatus = undefined

-- Queries --

selectMinQid :: MessageTable op -> Select (Field SqlInt4)
selectMinQid table = qid <$> Agg.aggregate
  (pMessageQueue $ emptyAggMessageQueue { qid = Agg.min })
  (selectTable table)

selectMaxQid :: MessageTable op -> Select (Field SqlInt4)
selectMaxQid table = qid <$> Agg.aggregate
  (pMessageQueue $ emptyAggMessageQueue { qid = Agg.max })
  (selectTable table)

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

{--

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MQueue
    qid Int
    config Text
    createdTime UTCTime default=CURRENT_TIME
    UniqueQid qid
    deriving Show
|]

type DBConnection = Pool SqlBackend

catchMaybe :: MonadUnliftIO io => io (Maybe a) -> io (Maybe a)
catchMaybe = flip catchAny $ \_ -> return Nothing

-- TODO: change DB location
_init :: MonadUnliftIO io => io DBConnection
_init = do
    pool <- runNoLoggingT $ createSqlitePool "db.sqlite3" 3
    let truncate = deleteWhere ([] :: [Filter MQueue])
    runSqlPool (runMigration migrateAll >> truncate) pool
    return pool

popFrontMessage :: MonadUnliftIO io => DBConnection -> io (Maybe (Int, String))
popFrontMessage = runSqlPool $ catchMaybe executeSql  where
    -- fmap over ReaderT, then over Maybe
    executeSql = getData <<$>> selectFirst [] [Desc MQueueCreatedTime]
    getData (Entity _ row) = (mQueueQid row, toString . mQueueConfig $ row) :: (Int, String)

-- TODO: return remainingQueue
enqueue :: MonadUnliftIO io => Int -> String -> DBConnection -> io (Maybe Int)
enqueue qid config = runSqlPool $ catchMaybe $ do
    curtime <- liftIO getCurrentTime
    -- fmap over ReaderT, then over Maybe
    (fromIntegral . fromSqlKey) <<$>> insertUnique (MQueue qid (toText config) curtime)

dequeue :: MonadUnliftIO io => Int -> DBConnection -> io (Maybe Bool)
dequeue qid = runSqlPool $ catchMaybe $ do
    -- fmap over ReaderT, then over Maybe
    exists <- const True <<$>> getBy (UniqueQid qid)
    deleteBy (UniqueQid qid)
    return exists

-- [(qid, remaining count)]
getStatus :: MonadUnliftIO io => DBConnection -> io [(Int, Int)]
getStatus = runSqlPool $ do
    -- fmap over ReaderT, then over []
    qids <- mQueueQid . entityVal <<$>> selectList [] [Asc MQueueCreatedTime]
    return $ zip qids [1 ..]
--}

-- Opaleye helper functions --

nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f

qidOrZero :: [Qid] -> Qid
qidOrZero = fromMaybe 0 . listToMaybe

insertIsSuccess :: Int64 -> Bool
insertIsSuccess result = result > 0
