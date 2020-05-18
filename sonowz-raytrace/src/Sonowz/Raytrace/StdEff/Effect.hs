module Sonowz.Raytrace.StdEff.Effect
  ( StdEff
  , throw'
  , stdEffToIO
  )
where

import Sonowz.Raytrace.Imports

-- Do logging & error handling

type StdEff = '[Error SomeException]

throw' :: (Member (Error SomeException) r, Exception e) => e -> Sem r a
throw' = throw . toException 

stdEffToIO :: Member (Embed IO) r => Sem (Error SomeException : r) a -> Sem r a
stdEffToIO m = runError m >>= printException where
  printException (Left e) = error ("Caught in 'stdEffToIO' : " <> show e)
  printException (Right x) = return x
