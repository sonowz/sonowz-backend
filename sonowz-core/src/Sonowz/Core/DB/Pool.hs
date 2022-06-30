module Sonowz.Core.DB.Pool
  ( DBConnPool,
    DBEffects,
    createConnPool,
    withDBConn,
    withDBConnIO,
  )
where

import Control.Exception.Safe qualified as E
import Data.Pool (LocalPool, Pool (..), createPool, destroyResource, putResource, takeResource)
import Database.PostgreSQL.Simple (ConnectInfo, Connection, close, connect, query_)
import Polysemy.Resource (Resource, bracket)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect

newtype DBConnPool = DBConnPool (Pool Connection)

type DBEffects = Reader DBConnPool : Resource : Embed IO : StdEff

maxDBConn :: Int
maxDBConn = 10

createConnPool :: (MonadIO m, HasCallStack) => ConnectInfo -> m DBConnPool
createConnPool connInfo = unsafeLiftIO $ do
  logDebugIO $ "DB connection pool was created. (Max connection: " <> show maxDBConn <> ")"
  DBConnPool <$> createPool (connect connInfo) close 1 10 maxDBConn

withDBConnIO :: DBConnPool -> (Connection -> IO a) -> IO a
withDBConnIO (DBConnPool pool) action = E.bracket takeAction putAction doAction
  where
    takeAction = unsafeLiftIO (getWorkingConnection pool)
    putAction = unsafeLiftIO . uncurry (flip putResource)
    doAction = action . fst

withDBConn :: Members DBEffects r => (Connection -> Sem r a) -> Sem r a
withDBConn action = do
  DBConnPool pool <- ask
  let !takeAction = liftIO (getWorkingConnection pool)
      !putAction = liftIO . uncurry (flip putResource)
      !doAction = action . fst
  bracket takeAction putAction doAction

-- Check connection with "SELECT 1", and try to reconnect
getWorkingConnection :: HasCallStack => Pool Connection -> IO (Connection, LocalPool Connection)
getWorkingConnection pool = do
  (conn, localpool) <- takeResource pool
  E.tryAny (checkPing conn) >>= \case
    Left _ ->
      destroyResource pool localpool conn
        >> logInfoIO "DB connection closed. Reconnecting..."
        >> getWorkingConnection pool
    Right _ -> return (conn, localpool)
  where
    checkPing conn = query_ conn "SELECT 1" >>= checkResult
    checkResult l = case viaNonEmpty head (join l) of
      Just (1 :: Int) -> pass
      _ -> error "did not return 1"
