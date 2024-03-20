module Sonowz.Core.StdEff.Effect
  ( module Sonowz.Core.StdEff.Effect.Log,
    StdEff,
    webLiftIO,
    stdEffToIOFinal,
  )
where

import Control.Exception.Safe qualified as E
import Relude.Extra.Bifunctor (firstF)
import Servant (ServerError (errBody), err500)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log

type StdEff = '[StdLog]

-- TODO: move this to another file
-- wraps exception to ServerError
-- Example: withDBConn $ \conn -> webLiftIO (someQuery ...)
-- Warning: This exposes exception message to client side
webLiftIO :: Members '[Error ServerError, Embed IO] r => IO a -> Sem r a
webLiftIO = fromEitherM . mapToServerError
  where
    mapToServerError :: IO a -> IO (Either ServerError a)
    mapToServerError = firstF (\e -> err500 {errBody = show e}) . E.tryAny

-- This catches all exceptions, including asynchronous exceptions
stdEffToIOFinal ::
  (Member (Final IO) r, HasCallStack) => Sem (StdLog : r) a -> Sem r a
stdEffToIOFinal m =
  ((errorToIOFinal . try . fromExceptionSem @SomeException . raise) m >>= printException . join)
    & raiseUnder
    & runStdLogIO
    & embedToFinal
  where
    printException (Left e) = error ("Caught in 'stdEffToIO' : " <> toText (displayException e))
    printException (Right x) = return x
