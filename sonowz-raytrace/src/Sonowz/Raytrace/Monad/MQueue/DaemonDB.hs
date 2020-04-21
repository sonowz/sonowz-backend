module Sonowz.Raytrace.Monad.MQueue.DaemonDB
  ( enqueueDaemonDB
  , dequeueDaemonDB
  ) where

import Relude

import Sonowz.Raytrace.Monad.MQueue (WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage, DaemonOp(..), emptyMessage)


enqueueDaemonDB :: WithDb m => DaemonMessage -> m ()
enqueueDaemonDB = undefined

dequeueDaemonDB :: WithDb m => m (Maybe DaemonMessage)
dequeueDaemonDB = undefined
