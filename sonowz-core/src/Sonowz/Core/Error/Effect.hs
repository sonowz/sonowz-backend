module Sonowz.Core.Error.Effect
  ( runErrorAsLogging,
    unsafeErrorToIO,
    catchAnyException,
    foreverCatch,
  )
where

import Control.Exception.Safe qualified as E
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect (StdLog, logError, logException)

runErrorAsLogging :: forall e r a. (Member StdLog r, Exception e, Monoid a, HasCallStack) => Sem (Error e : r) a -> Sem r a
runErrorAsLogging =
  runError >=> \case
    Left e -> logException e >> mempty
    Right a -> pure a

unsafeErrorToIO :: forall e r a. (Member (Embed IO) r, Exception e, HasCallStack) => Sem (Error e : r) a -> Sem r a
unsafeErrorToIO =
  runError >=> \case
    Left e -> liftIO $ E.throw e
    Right a -> pure a

-- | This catches all exceptions, including asynchronous exceptions
--
-- /Warning/: @Error e@ effects should be stacked above this
catchAnyException :: (Members [Final IO, StdLog] r, Monoid a) => Sem r a -> Sem r a
catchAnyException = errorToIOFinalAsLogging @SomeException . fromExceptionSem . raise
  where
    errorToIOFinalAsLogging :: forall e r a. (Members [Final IO, StdLog] r, Exception e, Monoid a) => Sem (Error e : r) a -> Sem r a
    errorToIOFinalAsLogging =
      errorToIOFinal >=> \case
        Left e -> logError "Unhandled exception in 'catchAnyException':" >> logException e >> mempty
        Right a -> pure a

-- | This catches all exceptions, including asynchronous exceptions
--
-- /Warning/: @Error e@ effects should be stacked above this
foreverCatch :: (Members [Final IO, StdLog] r, HasCallStack) => Sem r a -> Sem r Void
foreverCatch = infinitely . catchAnyException . void
