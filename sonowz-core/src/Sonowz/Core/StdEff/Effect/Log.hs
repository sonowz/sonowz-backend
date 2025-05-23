{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.StdEff.Effect.Log
  ( StdLog,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logException,
    logDebugIO,
    logInfoIO,
    logWarningIO,
    logErrorIO,
    logExceptionIO,
    setStdLogActionLevel,
    Severity (..),
    runStdLogIO,
    ignoreStdLog,
  )
where

import Colog.Core.Action (LogAction (..), hoistLogAction)
import Colog.Core.Severity (Severity (..), filterBySeverity)
import Control.Concurrent (ThreadId, myThreadId)
import Data.Time.LocalTime (ZonedTime)
import GHC.IO (unsafePerformIO)
import GHC.Stack (SrcLoc (..))
import Sonowz.Core.Imports
import Sonowz.Core.Time.Effect (Time, getTime, timeToIOFinal)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (..),
    setSGRCode,
  )

-- TODO: fix callstack off-by-one error

-- StdLog Effect --

data StdLog m a where
  LogDebug :: (HasCallStack) => Text -> StdLog m ()
  LogInfo :: (HasCallStack) => Text -> StdLog m ()
  LogWarning :: (HasCallStack) => Text -> StdLog m ()
  LogError :: (HasCallStack) => Text -> StdLog m ()
  LogException :: (HasCallStack, Exception e) => e -> StdLog m ()

makeSem ''StdLog

runStdLogIO :: (Member (Final IO) r) => Sem (StdLog : r) a -> Sem r a
runStdLogIO =
  interpret
    $ timeToIOFinal
    <$> \case
      LogDebug text -> withFrozenCallStack $ log Debug text
      LogInfo text -> withFrozenCallStack $ log Info text
      LogWarning text -> withFrozenCallStack $ log Warning text
      LogError text -> withFrozenCallStack $ log Error text
      LogException e -> withFrozenCallStack $ log Error (show $ displayException e)

ignoreStdLog :: Sem (StdLog : r) a -> Sem r a
ignoreStdLog = interpret $ \case
  LogDebug _ -> pass
  LogInfo _ -> pass
  LogWarning _ -> pass
  LogError _ -> pass
  LogException _ -> pass

log :: (Members '[Time, Final IO] r, HasCallStack) => Severity -> Text -> Sem r ()
log sev text = withFrozenCallStack $ makeStdMessage sev text >>= unLogAction stdLogAction

-- IO Logging --

logIO :: (HasCallStack) => Severity -> Text -> IO ()
logIO sev text = withFrozenCallStack $ makeStdMessageIO sev text >>= unLogAction stdLogActionIO

logDebugIO :: (HasCallStack) => Text -> IO ()
logDebugIO = withFrozenCallStack $ logIO Debug

logInfoIO :: (HasCallStack) => Text -> IO ()
logInfoIO = withFrozenCallStack $ logIO Info

logWarningIO :: (HasCallStack) => Text -> IO ()
logWarningIO = withFrozenCallStack $ logIO Warning

logErrorIO :: (HasCallStack) => Text -> IO ()
logErrorIO = withFrozenCallStack $ logIO Error

logExceptionIO :: (HasCallStack) => (Exception e) => e -> IO ()
logExceptionIO e = withFrozenCallStack $ logIO Error (show $ displayException e)

-- LogAction Type --

stdLogActionLevelRef :: IORef Severity
stdLogActionLevelRef = unsafePerformIO $ newIORef Debug
{-# NOINLINE stdLogActionLevelRef #-}

-- Use this function to change logging level
setStdLogActionLevel :: Severity -> IO ()
setStdLogActionLevel = writeIORef stdLogActionLevelRef

-- This function has 'unsafePerformIO' inside..
stdLogAction :: (Member (Final IO) r) => LogAction (Sem r) StdMessage
stdLogAction = filterBySeverity logLevel stdMessageSeverity logAction
  where
    logLevel = unsafePerformIO $ readIORef stdLogActionLevelRef
    logAction = fmtStdMessage >$< LogAction (embedFinal <$> putTextLn)
{-# NOINLINE stdLogAction #-}

stdLogActionIO :: LogAction IO StdMessage
stdLogActionIO = hoistLogAction runFinal stdLogAction

-- Message Type --

data StdMessage = StdMessage
  { stdMessageText :: Text,
    stdMessageSeverity :: Severity,
    stdMessageCallStack :: CallStack,
    stdMessageThreadId :: ThreadId,
    stdMessageTime :: ZonedTime
  }

makeStdMessage ::
  (Members '[Time, Final IO] r, HasCallStack) => Severity -> Text -> Sem r StdMessage
makeStdMessage stdMessageSeverity stdMessageText = withFrozenCallStack $ do
  let stdMessageCallStack = callStack
  stdMessageThreadId <- embedFinal myThreadId
  stdMessageTime <- getTime
  return StdMessage {..}

makeStdMessageIO :: (HasCallStack) => Severity -> Text -> IO StdMessage
makeStdMessageIO sev text = withFrozenCallStack (makeStdMessage sev text & timeToIOFinal & runFinal)

fmtStdMessage :: StdMessage -> Text
fmtStdMessage StdMessage {..} =
  withPadding 34 (show stdMessageTime)
    <> showSourceLoc stdMessageCallStack
    <> " "
    <> showThreadId stdMessageThreadId
    <> " "
    <> showSeverity stdMessageSeverity
    <> ": "
    <> stdMessageText

braceWithPadding :: Int -> Text -> Text
braceWithPadding padding text = "[" <> withPadding padding text <> "]"

withPadding :: Int -> Text -> Text
withPadding padding text = text <> stimes n " " where n = max 1 (padding - lengthText text)

sourcePaddingCount :: Int
sourcePaddingCount = 65

showSeverity :: Severity -> Text
showSeverity = \case
  Debug -> color Blue "DEBUG   "
  Info -> color Green "INFO    "
  Warning -> color Yellow "WARNING "
  Error -> color Red "ERROR   "
  where
    color :: Color -> Text -> Text
    color c txt =
      toText (setSGRCode [SetColor Foreground Vivid c]) <> txt <> toText (setSGRCode [Reset])

showSourceLoc :: CallStack -> Text
showSourceLoc cs = braceWithPadding sourcePaddingCount showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
      -- TODO: fix callstack inconsistency between 'log' effect and 'logIO'
      (_, loc) : (callerName, _) : _ | srcLocModule loc /= "Sonowz.Core.StdEff.Effect.Log" -> showLoc callerName loc
      _ : (_, loc) : (callerName, _) : _ -> showLoc callerName loc
      _ -> "<unknown loc>"

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc {..} =
      toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

showThreadId :: ThreadId -> Text
showThreadId = braceWithPadding 14 . show
