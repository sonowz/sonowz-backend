module Sonowz.Raytrace.Monad.MQueue.DaemonDB
  ( enqueueDaemonDB
  , enqueueDaemonDBNew
  , dequeueDaemonDB
  )
where

import Relude

import Sonowz.Raytrace.Monad.MQueue (WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (MessageQueue(..), DaemonMessage, DaemonOp, ServantId)
import Sonowz.Raytrace.Monad.MQueue.Db.Queries (enqueueDaemon, enqueueDaemonNew, dequeueDaemon)
import Sonowz.Raytrace.Monad.MQueue.Db.QueryUtil (grabConn, boolToException, maybeToException)

enqueueDaemonDB :: WithDb m => DaemonMessage -> m ()
enqueueDaemonDB msg =
  boolToException "enqueueDaemonDB"
    $   grabConn
    >>= (\conn -> liftIO $ enqueueDaemon conn (servantId msg) (operation msg))

dequeueDaemonDB :: WithDb m => m (Maybe DaemonMessage)
dequeueDaemonDB = grabConn >>= liftIO . dequeueDaemon

enqueueDaemonDBNew :: WithDb m => DaemonOp -> m ServantId
enqueueDaemonDBNew op =
  maybeToException "enqueueDaemonDBNew" $ grabConn >>= liftIO . flip enqueueDaemonNew op
