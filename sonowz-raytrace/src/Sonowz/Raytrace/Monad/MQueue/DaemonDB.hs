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
import Sonowz.Raytrace.Monad.MQueue.Db.QueryUtil (grabPool, boolToException, maybeToException)

enqueueDaemonDB :: WithDb m => DaemonMessage -> m ()
enqueueDaemonDB msg =
  boolToException "enqueueDaemonDB"
    $   grabPool
    >>= (\pool -> liftIO $ enqueueDaemon pool (servantId msg) (operation msg))

dequeueDaemonDB :: WithDb m => m (Maybe DaemonMessage)
dequeueDaemonDB = grabPool >>= liftIO . dequeueDaemon

enqueueDaemonDBNew :: WithDb m => DaemonOp -> m ServantId
enqueueDaemonDBNew op =
  maybeToException "enqueueDaemonDBNew" $ grabPool >>= liftIO . flip enqueueDaemonNew op
