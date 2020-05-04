module Sonowz.Raytrace.Monad.MQueueThread
  ( runMQueueThread
  , runHeteroMQueueThread
  , ThreadHandler
  , HandlerResult(..)
  , WithMQueues
  , WithHeteroMQueues
  )
where

import Relude
import UnliftIO.Concurrent (threadDelay)

import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))


type ThreadHandler m rx tx = rx -> m (HandlerResult tx)
data HandlerResult tx = HSend tx | HSendTerminate tx | HContinue | HTerminate

type WithMQueues (m :: * -> *) rx tx = (MonadIO m, MonadMQueue rx m, MonadMQueue tx m)
type WithHeteroMQueues (rm :: * -> *) (tm :: * -> *) rx tx
  = (MonadIO rm, MonadIO tm, MonadMQueue rx rm, MonadMQueue tx tm)

runMQueueThread :: WithMQueues m rx tx => ThreadHandler m rx tx -> m ()
runMQueueThread = runHeteroMQueueThread id

runHeteroMQueueThread
  :: WithHeteroMQueues rm tm rx tx => (forall a . rm a -> tm a) -> ThreadHandler tm rx tx -> tm ()
runHeteroMQueueThread hoist handler = do
  message <- hoist dequeueBlocking
  result  <- handler message
  case result of
    HContinue              -> runHeteroMQueueThread hoist handler
    HTerminate             -> pass
    HSend          message -> enqueue message >> runHeteroMQueueThread hoist handler
    HSendTerminate message -> enqueue message >> pass

dequeueBlocking :: (MonadIO m, MonadMQueue rx m) => m rx
dequeueBlocking = dequeue >>= \case
  Just message -> return message
  Nothing      -> threadDelay (10^6) >> dequeueBlocking
