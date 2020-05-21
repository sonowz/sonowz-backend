module Sonowz.Raytrace.StdEff.Effect
  ( module Sonowz.Raytrace.StdEff.Effect.Log
  , StdEff
  , throw'
  , onExceptionPrint
  , stdEffToIO
  )
where

import qualified Control.Exception.Safe as E

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.StdEff.Effect.Log

type StdEff = '[Error SomeException, StdLog]

throw' :: (Member (Error SomeException) r, Exception e) => e -> Sem r a
throw' = throw . toException

onExceptionPrint :: Member (Embed IO) r => Sem r a -> Sem r a
onExceptionPrint action = withLowerToIO $ \lower _ ->
  E.withException (lower action) (\(e :: SomeException) -> print e)

stdEffToIO :: Member (Embed IO) r => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIO m = runStdLogIO $ runError m >>= printException where
  printException (Left e) = error ("Caught in 'stdEffToIO' : " <> show e)
  printException (Right x) = return x
