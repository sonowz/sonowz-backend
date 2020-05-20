module Sonowz.Raytrace.StdEff.Effect
  ( StdEff
  , throw'
  , onExceptionPrint
  , stdEffToIO
  )
where

import qualified Control.Exception.Safe as E

import Sonowz.Raytrace.Imports

-- Do logging & error handling

type StdEff = '[Error SomeException]

throw' :: (Member (Error SomeException) r, Exception e) => e -> Sem r a
throw' = throw . toException

onExceptionPrint :: Member (Embed IO) r => Sem r a -> Sem r a
onExceptionPrint action = withLowerToIO $ \lower _ ->
  E.withException (lower action) (\(e :: SomeException) -> print e)

stdEffToIO :: Member (Embed IO) r => Sem (Error SomeException : r) a -> Sem r a
stdEffToIO m = runError m >>= printException where
  printException (Left e) = error ("Caught in 'stdEffToIO' : " <> show e)
  printException (Right x) = return x
