module Sonowz.Raytrace.Monad.MQueue.DaemonDB () where

import Relude

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue (..), MQueueException (..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage, DaemonOp(..), emptyMessage)

instance WithDb m => MonadMQueue DaemonMessage m where
  enqueue = undefined -- enqueueDaemonDB
  dequeue = undefined -- enqueueDaemonDB
