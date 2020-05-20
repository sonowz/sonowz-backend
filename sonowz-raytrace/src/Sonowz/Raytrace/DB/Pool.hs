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
import qualified Control.Exception.Safe as E

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.StdEff.Effect (StdEff)

newtype DBConnPool = DBConnPool (Pool Connection)

type DBEffects = Reader DBConnPool : Resource : Embed IO : StdEff

createConnPool :: MonadIO m => ConnectInfo -> m DBConnPool
createConnPool connInfo = liftIO $ DBConnPool <$> createPool (connect connInfo) close 1 10 10

withDBConnIO :: DBConnPool -> (Connection -> IO a) -> IO a
withDBConnIO (DBConnPool pool) action = E.bracket takeAction putAction doAction where
  takeAction = liftIO $ takeResource pool
  putAction  = liftIO . uncurry (flip putResource)
  doAction   = action . fst

withDBConn :: Members DBEffects r => (Connection -> Sem r a) -> Sem r a
withDBConn action = do
  DBConnPool pool <- ask
  let
    !takeAction = liftIO (takeResource pool)
    !putAction  = liftIO . uncurry (flip putResource)
    !doAction   = action . fst
  bracket takeAction putAction doAction
