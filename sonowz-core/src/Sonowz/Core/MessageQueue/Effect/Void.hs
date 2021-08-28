module Sonowz.Core.MessageQueue.Effect.Void
  ( runMQueueVoid
  )
where

import Sonowz.Core.Imports
import Sonowz.Core.MessageQueue.Effect (MessageQueue(..))

runMQueueVoid :: Sem (MessageQueue Void : r) a -> Sem r a
runMQueueVoid = interpret $ \case
  Enqueue _ -> pass
  Dequeue -> error "Dequeuing from Void message queue"
