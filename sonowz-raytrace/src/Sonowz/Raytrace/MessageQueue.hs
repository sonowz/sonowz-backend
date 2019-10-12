{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
module Sonowz.Raytrace.MessageQueue where

import           Relude
import           Control.Monad.Logger           ( runNoLoggingT )
import           Control.Monad.IO.Unlift
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Aeson.TH
import           Data.Pool
import           Data.Time
import           UnliftIO.Exception

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
    getData (Entity _ row) =
        (mQueueQid row, show . mQueueConfig $ row) :: (Int, String)

-- TODO return remainingQueue
enqueue :: MonadUnliftIO io => Int -> String -> DBConnection -> io (Maybe Int)
enqueue qid config = runSqlPool $ catchMaybe $ do
    curtime <- liftIO getCurrentTime
    -- fmap over ReaderT, then over Maybe
    (fromIntegral . fromSqlKey)
        <<$>> insertUnique (MQueue qid (toText config) curtime)

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
