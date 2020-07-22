{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Core.StdEff.Effect.Log
  ( StdLog
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  , logDebugIO
  , logInfoIO
  , logWarningIO
  , logErrorIO
  , logExceptionIO
  , setStdLogActionLevel
  , runStdLogIO
  )
where

import Colog.Core.Action (LogAction(..), hoistLogAction)
import Colog.Core.Severity (Severity(..), filterBySeverity)
import Control.Concurrent (ThreadId, myThreadId)
import Data.Time.LocalTime (ZonedTime)
import GHC.Stack (SrcLoc(..))
import GHC.IO (unsafePerformIO)
import System.Console.ANSI
  (Color(..), ColorIntensity(Vivid), ConsoleLayer(Foreground), SGR(..), setSGRCode)

import Sonowz.Core.Imports
import Sonowz.Core.Time.Effect (Time, getTime, timeToIO)


-- StdLog Effect --

data StdLog m a where
  LogDebug ::HasCallStack => Text -> StdLog m ()
  LogInfo ::HasCallStack => Text -> StdLog m ()
  LogWarning ::HasCallStack => Text -> StdLog m ()
  LogError ::HasCallStack => Text -> StdLog m ()
  LogException ::(HasCallStack, Exception e) => e -> StdLog m ()

makeSem ''StdLog

runStdLogIO :: Member (Embed IO) r => Sem (StdLog : r) a -> Sem r a
runStdLogIO = interpret $ timeToIO <$> \case
  LogDebug     text -> withFrozenCallStack $ log Debug text
  LogInfo      text -> withFrozenCallStack $ log Info text
  LogWarning   text -> withFrozenCallStack $ log Warning text
  LogError     text -> withFrozenCallStack $ log Error text
  LogException e    -> withFrozenCallStack $ log Error (show $ displayException e)

log :: (Members '[Time, Embed IO] r, HasCallStack) => Severity -> Text -> Sem r ()
log sev text = withFrozenCallStack $ makeStdMessage sev text >>= unLogAction stdLogAction


-- IO Logging --

logIO :: HasCallStack => Severity -> Text -> IO ()
logIO sev text = withFrozenCallStack $ makeStdMessageIO sev text >>= unLogAction stdLogActionIO

logDebugIO :: HasCallStack => Text -> IO ()
logDebugIO = withFrozenCallStack . logIO Debug

logInfoIO :: HasCallStack => Text -> IO ()
logInfoIO = withFrozenCallStack . logIO Info

logWarningIO :: HasCallStack => Text -> IO ()
logWarningIO = withFrozenCallStack . logIO Warning

logErrorIO :: HasCallStack => Text -> IO ()
logErrorIO = withFrozenCallStack . logIO Error

logExceptionIO :: HasCallStack => Exception e => e -> IO ()
logExceptionIO e = withFrozenCallStack $ logIO Error (show $ displayException e)


-- LogAction Type --

stdLogActionLevelRef :: IORef Severity
stdLogActionLevelRef = unsafePerformIO $ newIORef Debug
{-# NOINLINE stdLogActionLevelRef #-}

-- Use this function to change logging level
setStdLogActionLevel :: Severity -> IO ()
setStdLogActionLevel = writeIORef stdLogActionLevelRef

-- This function has 'unsafePerformIO' inside..
stdLogAction :: Member (Embed IO) r => LogAction (Sem r) StdMessage
stdLogAction = filterBySeverity logLevel stdMessageSeverity logAction where
  logLevel  = unsafePerformIO $ readIORef stdLogActionLevelRef
  logAction = fmtStdMessage >$< LogAction putTextLn
{-# NOINLINE stdLogAction #-}

stdLogActionIO :: LogAction IO StdMessage
stdLogActionIO = hoistLogAction runM stdLogAction


-- Message Type --

data StdMessage = StdMessage
  { stdMessageText :: Text
  , stdMessageSeverity :: Severity
  , stdMessageCallStack :: CallStack
  , stdMessageThreadId :: ThreadId
  , stdMessageTime :: ZonedTime
}

makeStdMessage
  :: (Members '[Time, Embed IO] r, HasCallStack) => Severity -> Text -> Sem r StdMessage
makeStdMessage stdMessageSeverity stdMessageText = withFrozenCallStack $ do
  let stdMessageCallStack = callStack
  stdMessageThreadId <- liftIO myThreadId
  stdMessageTime     <- getTime
  return StdMessage { .. }

makeStdMessageIO :: HasCallStack => Severity -> Text -> IO StdMessage
makeStdMessageIO sev text = withFrozenCallStack (makeStdMessage sev text & timeToIO & runM)

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
  Debug   -> color Green "DEBUG   "
  Info    -> color Blue "INFO    "
  Warning -> color Yellow "WARNING "
  Error   -> color Red "ERROR   "
 where
  color :: Color -> Text -> Text
  color c txt =
    toText (setSGRCode [SetColor Foreground Vivid c]) <> txt <> toText (setSGRCode [Reset])

showSourceLoc :: CallStack -> Text
showSourceLoc cs = braceWithPadding sourcePaddingCount showCallStack
 where
  showCallStack :: Text
  showCallStack = case getCallStack cs of
    {-
    [] -> "<unknown loc>"
    [(name, loc)] -> showLoc name loc
    _ : [(name, loc)] -> showLoc name loc
    -}
    _ : (_, loc) : (callerName, _) : _ -> showLoc callerName loc
    _ -> "<unknown loc>"

  showLoc :: String -> SrcLoc -> Text
  showLoc name SrcLoc {..} =
    toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

showThreadId :: ThreadId -> Text
showThreadId = braceWithPadding 14 . show
