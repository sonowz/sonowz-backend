module Sonowz.Core.StdEff.Effect
  ( module Sonowz.Core.StdEff.Effect.Log
  , StdEff
  , throw'
  , stdEffToIO
  , stdEffToIOFinal
  ) where

import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log

-- Note: In order to catch all exceptions
-- including those not thrown by 'Polysemy.Error.throw', (e.g. 'Control.Exception.throw')
-- one should use `fromExceptionSem (catch _action_)`
-- rather than `catch _action_`.

type StdEff = '[Error SomeException , StdLog]

-- Utility function for 'Polysemy.Error.throw'
throw' :: forall e r a . (Member (Error SomeException) r, Exception e) => e -> Sem r a
throw' = throw . toException

-- This throws exceptions which are not thrown by 'Polysemy.Error.throw'
stdEffToIO :: Member (Embed IO) r => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIO m = runStdLogIO $ runError m >>= printException where
  printException (Left  e) = error ("Caught in 'stdEffToIO' : " <> show e)
  printException (Right x) = return x

-- This catches all kinds of synchronous exceptions
stdEffToIOFinal :: Member (Final IO) r => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIOFinal m =
  (errorToIOFinal (fromExceptionSem m) >>= printException) & raiseUnder & runStdLogIO & embedToFinal
 where
  printException (Left  e) = error ("Caught in 'stdEffToIO' : " <> show e)
  printException (Right x) = return x
