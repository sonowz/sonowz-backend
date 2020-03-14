{-# LANGUAGE UndecidableInstances #-}
module Sonowz.Raytrace.Monad.MQueue.DaemonDB () where

import Relude

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue (..), MQueueException (..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage)

instance WithDb r m => MonadMQueue m DaemonMessage where
  enqueue = undefined -- enqueueDaemonDB
  dequeue = undefined -- enqueueDaemonDB
