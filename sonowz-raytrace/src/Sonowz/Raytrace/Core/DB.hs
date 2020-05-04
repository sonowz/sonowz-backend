module Sonowz.Raytrace.Core.DB
  ( DBConnPool
  , createConnPool
  , withDBConn
  )
where

import Relude
import Data.Pool (Pool(..), createPool, takeResource, putResource)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close)
import UnliftIO (MonadUnliftIO(..), bracket)

newtype DBConnPool = DBConnPool (Pool Connection)

createConnPool :: MonadIO m => ConnectInfo -> m DBConnPool
createConnPool connInfo = liftIO $ DBConnPool <$> createPool (connect connInfo) close 1 10 5

withDBConn :: MonadUnliftIO m => DBConnPool -> (Connection -> m a) -> m a
withDBConn (DBConnPool pool) action = bracket takeAction putAction doAction where
  takeAction = liftIO $ takeResource pool
  putAction  = liftIO . uncurry (flip putResource)
  doAction   = action . fst
