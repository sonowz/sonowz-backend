module Sonowz.Raytrace.Monad.MQueue.DaemonState () where

import Relude

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue (..), MQueueException (..))
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage)

instance Monad m => MonadMQueue DaemonMessage (StateT [DaemonMessage] m) where
  enqueue = undefined -- enqueueDaemonST
  dequeue = undefined -- dequeueDaemonST
