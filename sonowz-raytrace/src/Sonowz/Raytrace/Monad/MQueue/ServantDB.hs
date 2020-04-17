module Sonowz.Raytrace.Monad.MQueue.ServantDB
  ( withServantQueue
  , ServantQT(..)
  , ServantEnv(..)
  ) where

import Relude
import UnliftIO (MonadUnliftIO(..))

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue (..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (ServantMessage, ServantOp(..), ServantId(..))

instance WithDb m => MonadMQueue ServantMessage (ServantQT env m) where
  enqueue = undefined -- enqueueServantDB
  dequeue = undefined -- dequeueServantDB

newtype ServantQT env (m :: * -> *) a = ServantQT { runServantQT :: ServantEnv env -> m a }
  deriving (Generic, Functor, Applicative, Monad)

data ServantEnv env = ServantEnv ServantId env

withServantQueue :: ServantId -> env -> ServantQT env m a -> m a
withServantQueue sid env = flip runServantQT (ServantEnv sid env)

instance Monad m => MonadReader (ServantEnv env) (ServantQT env m) where
  ask = ServantQT return
  local f m = ServantQT $ runServantQT m . f

instance MonadTrans (ServantQT env) where
  lift m = ServantQT (const m)

instance MonadIO m => MonadIO (ServantQT env m) where
  liftIO = lift . liftIO

instance MonadUnliftIO m => MonadUnliftIO (ServantQT env m) where
  withRunInIO inner =
    ServantQT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runServantQT r)

instance {-# OVERLAPPING #-} Monad m => MonadHas ServantId (ServantQT env m) where
  grab = (\(ServantEnv sid _) -> sid) <$> ask

instance (Monad m, Has field env) => MonadHas field (ServantQT env m) where
  grab = (\(ServantEnv _ env) -> obtain env) <$> ask

-- enqueueServantDB :: WithDb r m => ServantMessage -> m (Either MQueueException ())
