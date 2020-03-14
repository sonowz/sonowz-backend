{-# LANGUAGE UndecidableInstances #-}
module Sonowz.Raytrace.Monad.MQueue.ServantDB () where

import Relude

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue (..), MQueueException (..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (ServantMessage)

instance WithDb r m => MonadMQueue m ServantMessage where
  enqueue = undefined -- enqueueServantDB
  dequeue = undefined -- dequeueServantDB

-- enqueueServantDB :: WithDb r m => ServantMessage -> m (Either MQueueException ())
