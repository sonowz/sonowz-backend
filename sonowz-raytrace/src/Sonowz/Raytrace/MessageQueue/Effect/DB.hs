module Sonowz.Raytrace.MessageQueue.Effect.DB
  ( runMQueueDBServant,
    runMQueueDBDaemon,
    enqueueDBDaemonNew,
  )
where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.DB.Utils (boolToException, maybeToException)
import Sonowz.Core.MessageQueue.Effect (MessageQueue (..))
import Sonowz.Raytrace.DB.Queries
  ( dequeueDaemon,
    dequeueServant,
    enqueueDaemon,
    enqueueDaemonNew,
    enqueueServant,
  )
import Sonowz.Raytrace.DB.Types
  ( DaemonMessage,
    DaemonOp,
    Message (..),
    ServantId (..),
    ServantMessage,
  )
import Sonowz.Raytrace.Imports

runMQueueDBServant ::
  Members (Reader ServantId : DBEffects) r => Sem (MessageQueue ServantMessage : r) a -> Sem r a
runMQueueDBServant = interpret $ \case
  Enqueue msg ->
    boolToException "enqueueServantDB" $
      withDBConn (\conn -> liftIO $ enqueueServant conn (servantId msg) (operation msg))
  Dequeue -> do
    servantId' <- ask @ServantId
    withDBConn (\conn -> liftIO $ dequeueServant conn servantId')

runMQueueDBDaemon :: Members DBEffects r => Sem (MessageQueue DaemonMessage : r) a -> Sem r a
runMQueueDBDaemon = interpret $ \case
  Enqueue msg ->
    boolToException "enqueueDaemonDB" $
      withDBConn (\conn -> liftIO $ enqueueDaemon conn (servantId msg) (operation msg))
  Dequeue -> withDBConn (liftIO . dequeueDaemon)

enqueueDBDaemonNew :: Members DBEffects r => DaemonOp -> Sem r ServantId
enqueueDBDaemonNew op =
  maybeToException "enqueueDaemonDBNew" $ withDBConn (liftIO . flip enqueueDaemonNew op)
