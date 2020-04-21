module Sonowz.Raytrace.Monad.MQueue.IORef
  ( IORefQueue
  , enqueueIORef
  , dequeueIORef
  , removeIORef
  )
where

import Relude
import Data.List (partition)

import Sonowz.Raytrace.Core.Has (MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (ServantId, DaemonMessage)
import Sonowz.Raytrace.RaytraceConfig (Config(..))

type IORefQueue a = IORef [a]

grabIORef :: MonadHas (IORefQueue a) m => m (IORefQueue a)
grabIORef = grab

enqueueIORef :: (MonadHas (IORefQueue a) m, MonadIO m) => a -> m ()
enqueueIORef x = grabIORef >>= flip modifyIORef' f where f = flip (<>) [x]

dequeueIORef :: (MonadHas (IORefQueue a) m, MonadIO m, Functor m) => m (Maybe a)
dequeueIORef = grabIORef >>= flip atomicModifyIORef' f where
  f (x : xs) = (xs, Just x)
  f l        = (l, Nothing)

-- Returns False if no item were removed
removeIORef :: (MonadHas (IORefQueue a) m, MonadIO m) => (a -> Bool) -> m Bool
removeIORef p = grabIORef >>= flip atomicModifyIORef' f
  where f l = let (rs, xs) = partition p l in (xs, not $ null rs)
