module Sonowz.Core.StdEff.Effect
  ( module Sonowz.Core.StdEff.Effect.Log,
    StdEff,
    webLiftIO,
    stdEffToWebHandler,
    stdEffToIOFinal,
  )
where

import Control.Exception.Safe qualified as E
import Relude.Extra.Bifunctor (firstF)
import Relude.Monad.Reexport (ExceptT (ExceptT))
import Servant (Handler (Handler), ServerError (errBody), err500)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log

type StdEff = '[StdLog]

-- TODO: move this to another file

-- | Wraps exception to ServerError
--
-- Example: @withDBConn $ \conn -> webLiftIO (someQuery ...)@
--
-- /Warning/: This exposes exception message to client side
webLiftIO :: Members '[Error ServerError, Embed IO] r => IO a -> Sem r a
webLiftIO = fromEitherM . mapToServerError
  where
    mapToServerError :: IO a -> IO (Either ServerError a)
    mapToServerError = firstF (\e -> err500 {errBody = show e}) . E.tryAny

-- | Entry point for web handlers
stdEffToWebHandler :: Sem _ a -> Handler a
stdEffToWebHandler (m :: Members (Error ServerError : Final IO : StdEff) r => Sem r a) =
  m
    & errorToIOFinal @ServerError
    & stdEffToIOFinal
    & runFinal @IO
    & logServerError
    & (Handler . ExceptT)
  where
    logServerError :: IO (Either ServerError a) -> IO (Either ServerError a)
    logServerError action = action >>= bitraverse (\e -> logInfoIO (show e) >> return e) return

-- | This catches all exceptions, including asynchronous exceptions
stdEffToIOFinal ::
  (Member (Final IO) r, HasCallStack) => Sem (StdLog : r) a -> Sem r a
stdEffToIOFinal m =
  m
    & (errorToIOFinalAsLogging @SomeException . fromExceptionSem . raise)
    & raiseUnder
    & runStdLogIO
    & embedToFinal
  where
    errorToIOFinalAsLogging :: forall e r a. (Members [Final IO, StdLog] r, Exception e) => Sem (Error e : r) a -> Sem r a
    errorToIOFinalAsLogging =
      errorToIOFinal >=> \case
        Left e -> logError "Unhandled exception in 'stdEffToIO':" >> logException e >> error (toText . displayException $ e)
        Right a -> pure a
