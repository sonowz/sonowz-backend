module Sonowz.Raytrace.Monad.MQueue.RunInfoIORef
  ( RunInfo(..)
  , RunInfoQueue(..)
  , remove
  )
where

import Relude

import Sonowz.Raytrace.Core.Has (MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (ServantId, DaemonMessage)
import Sonowz.Raytrace.RaytraceConfig (Config(..))

data RunInfo = RunInfo ServantId Config deriving (Show, Read)
type RunInfoQueue = IORef [RunInfo]
newtype IORefQueue a = IORefQueue a

-- TODO: fix typecheck error
{- instance (MonadHas RunInfoQueue m, MonadIO m) => MonadMQueue RunInfo m where
  enqueue = undefined -- enqueueDaemonST
  dequeue = undefined -- dequeueDaemonST
 -}
 
-- Returns False if no item were removed
remove :: MonadHas RunInfoQueue m => (a -> Bool) -> m Bool
remove = undefined
