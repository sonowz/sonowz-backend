module Sonowz.Raytrace.Monad.MQueue
  ( MonadMQueue (..)
  , MQueueException (..)
  , WithDb
  ) where

import Relude
import Control.Exception.Safe (Exception, MonadThrow)
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..))

-- Monad interface for message queue
class Monad m => MonadMQueue m msg where
  enqueue :: msg -> m ()
  dequeue :: m (Maybe msg)

data MQueueException = MQueueException Text deriving (Show, Exception)

type WithDb r m = (MonadReader r m, Has PGS.Connection r, MonadIO m, MonadThrow m)
