module Sonowz.Core.Error.Effect
  ( runErrorAsLogging,
    mapErrorAs500,
    unsafeErrorToIO,
    catchAnyException,
    foreverCatch,
  )
where

import Control.Exception.Safe qualified as E
import Servant (ServerError (errBody), err500)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect (StdLog, logError, logException)

runErrorAsLogging :: forall e r a. (Member StdLog r, Exception e, Monoid a, HasCallStack) => Sem (Error e : r) a -> Sem r a
runErrorAsLogging =
  runError >=> \case
    Left e -> logException e >> mempty
    Right a -> pure a

mapErrorAs500 :: forall e r a. (Member (Error ServerError) r, Exception e, HasCallStack) => Sem (Error e : r) a -> Sem r a
mapErrorAs500 = mapError (\e -> err500 {errBody = show e})

unsafeErrorToIO :: forall e r a. (Member (Embed IO) r, Exception e, HasCallStack) => Sem (Error e : r) a -> Sem r a
unsafeErrorToIO =
  runError >=> \case
    Left e -> liftIO $ E.throw e
    Right a -> pure a

-- | This catches all exceptions, including asynchronous exceptions
--
-- /Warning/: @Error e@ effects should be stacked above this
catchAnyException :: (Members [Final IO, StdLog] r, Monoid a) => Sem r () -> Sem r a -> Sem r a
catchAnyException callback = errorToIOFinalAsLogging @SomeException callback . fromExceptionSem . raise
  where
    errorToIOFinalAsLogging :: forall e r a. (Members [Final IO, StdLog] r, Exception e, Monoid a) => Sem r () -> Sem (Error e : r) a -> Sem r a
    errorToIOFinalAsLogging callback =
      errorToIOFinal >=> \case
        Left e -> logError "Unhandled exception in 'catchAnyException':" >> logException e >> callback >> mempty
        Right a -> pure a

-- | This catches all exceptions, including asynchronous exceptions
--
-- /Warning/: @Error e@ effects should be stacked above this
foreverCatch :: (Members [Final IO, StdLog] r, HasCallStack) => Sem r () -> Sem r a -> Sem r Void
foreverCatch callback = infinitely . catchAnyException callback . void
