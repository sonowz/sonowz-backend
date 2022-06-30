module Sonowz.Core.MessageQueue.Effect.State
  ( runMQueueState,
    removeMQueueState,
  )
where

import Data.List (partition)
import Sonowz.Core.Imports
import Sonowz.Core.MessageQueue.Effect (MessageQueue (..))

runMQueueState :: Sem (MessageQueue msg : r) a -> Sem (AtomicState [msg] : r) a
runMQueueState = reinterpret $ \case
  Enqueue msg -> atomicState' $ \l -> (l <> [msg], ())
  Dequeue -> atomicState' f
    where
      f (msg : msgs) = (msgs, Just msg)
      f l = (l, Nothing)

-- Returns False if no item were removed
removeMQueueState :: Member (AtomicState [msg]) r => (msg -> Bool) -> Sem r Bool
removeMQueueState p = atomicState' f
  where
    f l = let (rs, xs) = partition p l in (xs, not $ null rs)
