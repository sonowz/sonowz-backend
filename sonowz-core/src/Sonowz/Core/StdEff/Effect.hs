module Sonowz.Core.StdEff.Effect
  ( module Sonowz.Core.StdEff.Effect.Log,
    StdEff,
    throw',
    liftIO,
    webLiftIO,
    stdEffToIO,
    stdEffToIOFinal,
  )
where

import Servant (ServerError (errBody), err500)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log

-- Note: In order to catch all exceptions
-- including those not thrown by 'Polysemy.Error.throw', (e.g. 'Control.Exception.throw')
-- one should use `fromExceptionSem (catch _action_)`
-- rather than `catch _action_`.

type StdEff = '[Error SomeException, StdLog]

-- Utility function for 'Polysemy.Error.throw'
throw' :: forall e r a. (Member (Error SomeException) r, Exception e) => e -> Sem r a
throw' = throw . toException

-- This embraces all IO exceptions into 'Error SomeException' effect
-- The original function was renamed as 'unsafeLiftIO' in 'Sonowz.Core.Imports'
liftIO :: Members '[Error SomeException, Embed IO] r => IO a -> Sem r a
liftIO = fromException

-- 'liftIO' variant in web servers
-- Example: withDBConn $ \conn -> webLiftIO (someQuery ...)
-- Warning: This exposes exception message to client side
webLiftIO :: Members '[Error ServerError, Embed IO] r => IO a -> Sem r a
webLiftIO = fromExceptionVia (\(e :: SomeException) -> err500 {errBody = show e})

-- This throws exceptions which are not thrown by 'Polysemy.Error.throw'
stdEffToIO ::
  (Member (Embed IO) r, HasCallStack) => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIO m = runStdLogIO $ runError m >>= printException
  where
    printException (Left e) = error ("Caught in 'stdEffToIO' : " <> show e)
    printException (Right x) = return x

-- This catches all kinds of synchronous exceptions
stdEffToIOFinal ::
  (Member (Final IO) r, HasCallStack) => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIOFinal m =
  (errorToIOFinal (fromExceptionSem m) >>= printException) & raiseUnder & runStdLogIO & embedToFinal
  where
    printException (Left e) = error ("Caught in 'stdEffToIO' : " <> show e)
    printException (Right x) = return x
