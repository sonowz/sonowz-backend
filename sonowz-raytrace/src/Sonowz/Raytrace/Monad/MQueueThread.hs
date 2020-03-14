module Sonowz.Raytrace.Monad.MQueueThread 
  ( runMQueueThread
  , ThreadHandler
  , HandlerResult (..)
  , WithMQueues
  ) where

import Relude
import UnliftIO.Concurrent (threadDelay)

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))


type ThreadHandler m rx tx = rx -> m (HandlerResult tx)
data HandlerResult tx = HSend tx | HContinue | HTerminate

type WithMQueues (m :: * -> *) rx tx = (MonadIO m, MonadMQueue m rx, MonadMQueue m tx)

runMQueueThread :: WithMQueues m rx tx => ThreadHandler m rx tx -> m ()
runMQueueThread handler = do
  message <- dequeueBlocking
  result <- handler message
  case result of
    HContinue -> runMQueueThread handler
    HTerminate -> pass
    HSend message -> enqueue message >> runMQueueThread handler

dequeueBlocking :: (MonadIO m, MonadMQueue m rx) => m rx
dequeueBlocking = dequeue >>= \case
  Just message -> return message
  Nothing -> threadDelay 1000 >> dequeueBlocking