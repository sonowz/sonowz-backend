module Sonowz.Core.StdEff.Effect
  ( module Sonowz.Core.StdEff.Effect.Log,
    StdEff,
    webLiftIO,
    stdEffToIO,
    stdEffToIOFinal,
  )
where

import Control.Exception qualified as E
import Servant (ServerError (errBody), err500)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log

type StdEff = '[StdLog]

-- TODO: move this to another file
-- 'liftIO' variant in web servers
-- Example: withDBConn $ \conn -> webLiftIO (someQuery ...)
-- Warning: This exposes exception message to client side
webLiftIO :: Members '[Embed IO] r => IO a -> Sem r a
webLiftIO = liftIO $ (\(e :: SomeException) -> err500 {errBody = show e})

-- TODO: remove this function?
-- This throws exceptions which are not thrown by 'Polysemy.Error.throw'
stdEffToIO ::
  (Member (Embed IO) r, HasCallStack) => Sem (Error SomeException : StdLog : r) a -> Sem r a
stdEffToIO m = runStdLogIO $ runError m >>= printException
  where
    printException (Left e) = error ("Caught in 'stdEffToIO' : " <> toText (displayException e))
    printException (Right x) = return x

-- This catches all exceptions, including asynchronous exceptions
stdEffToIOFinal ::
  (Member (Final IO) r, HasCallStack) => Sem (StdLog : r) a -> Sem r a
stdEffToIOFinal m =
  ((errorToIOFinal . try . fromExceptionSem . raise) m >>= printException . join)
    & raiseUnder
    & runStdLogIO
    & embedToFinal
  where
    printException (Left e) = error ("Caught in 'stdEffToIO' : " <> toText (displayException e))
    printException (Right x) = return x
