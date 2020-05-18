module Sonowz.Raytrace.MessageQueue.Effect.Void
  ( runMQueueVoid
  )
where

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.MessageQueue.Effect (MessageQueue(..))

runMQueueVoid :: Sem (MessageQueue Void : r) a -> Sem r a
runMQueueVoid = interpret $ \case
  Enqueue _ -> pass
  Dequeue -> error "Dequeuing from Void message queue"
