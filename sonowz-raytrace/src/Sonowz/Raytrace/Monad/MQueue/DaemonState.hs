module Sonowz.Raytrace.Monad.MQueue.DaemonState
  ( StateQT
  , remove
  )
where

import Relude

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage)

type StateQT s m = StateT [s] m

instance Monad m => MonadMQueue s (StateQT s m) where
  enqueue = undefined -- enqueueDaemonST
  dequeue = undefined -- dequeueDaemonST

-- Returns False if no item were removed
remove :: MonadMQueue s (StateQT s m) => (s -> Bool) -> (StateQT s m) Bool
remove = undefined
