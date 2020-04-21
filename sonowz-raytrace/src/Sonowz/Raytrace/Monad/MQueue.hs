module Sonowz.Raytrace.Monad.MQueue
  ( MonadMQueue(..)
  , MQueueException(..)
  , WithDb
  )
where

import Relude
import UnliftIO.Exception (throwString)
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (MonadHas(..))

-- Monad interface for message queue
class Monad m => MonadMQueue msg m where
  enqueue :: msg -> m ()
  dequeue :: m (Maybe msg)

data MQueueException = MQueueException Text deriving (Show, Exception)

type WithDb m = (MonadHas PGS.Connection m, MonadIO m)

instance MonadIO m => MonadMQueue Void m where
  enqueue _ = pass
  dequeue = throwString "Dequeueing from Void instance"
