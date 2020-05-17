module Sonowz.Raytrace.DB.Pool
  ( DBConnPool
  , DBEffects
  , createConnPool
  , withDBConn
  , withDBConnIO
  )
where

import Data.Pool (Pool(..), createPool, takeResource, putResource)
import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close)
import Polysemy.Resource (Resource, bracket)
import UnliftIO (MonadUnliftIO(..))
import qualified UnliftIO as U

import Sonowz.Raytrace.Imports

newtype DBConnPool = DBConnPool (Pool Connection)

type DBEffects = [Reader DBConnPool, Resource, Embed IO]

createConnPool :: MonadIO m => ConnectInfo -> m DBConnPool
createConnPool connInfo = liftIO $ DBConnPool <$> createPool (connect connInfo) close 1 10 10

withDBConnIO :: MonadUnliftIO m => DBConnPool -> (Connection -> m a) -> m a
withDBConnIO (DBConnPool pool) action = U.bracket takeAction putAction doAction where
  takeAction = liftIO $ takeResource pool
  putAction  = liftIO . uncurry (flip putResource)
  doAction   = action . fst

withDBConn :: Members DBEffects r => (Connection -> Sem r a) -> Sem r a
withDBConn action = do
  DBConnPool pool <- ask
  let 
    takeAction = embed $ takeResource pool
    putAction  = embed . uncurry (flip putResource)
    doAction   = action . fst
  bracket takeAction putAction doAction
