module Sonowz.Core.Web.Wai
  ( runWithLog
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (Status(statusCode, statusMessage))
import Network.Wai (Application, Request(rawPathInfo, requestMethod))
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setLogger, setPort)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect.Log (logInfoIO)
import System.Console.ANSI

runWithLog :: HasCallStack => Port -> Application -> IO ()
runWithLog port = runSettings settings where
  settings = setPort port $ setLogger logger defaultSettings
  logger :: Request -> Status -> Maybe Integer -> IO ()
  logger req status fileSize =
    logInfoIO $ toText (setSGRCode [SetColor Foreground Dull Green]) <> text <> toText
      (setSGRCode [Reset])   where
    text =
      T.intercalate " "
        $  [ decodeUtf8 $ requestMethod req
           , decodeUtf8 $ rawPathInfo req
           , show $ statusCode status
           , decodeUtf8 $ statusMessage status
           ]
        <> (show <$> maybeToList fileSize)

