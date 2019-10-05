module Main where

import           Relude
import           System.IO
import           Control.Applicative
import           Control.Concurrent
import           Network.Wai.Handler.Warp
import           Servant

import qualified Sonowz.Raytrace.Types.API     as API
import qualified Sonowz.Raytrace.Daemon as RTDaemon
import qualified Sonowz.Raytrace.Index as Raytrace
import qualified Sonowz.Raytrace.Websocket as RTWebsocket

api :: Proxy API.API
api = Proxy

server :: RTWebsocket.WSData -> Server API.API
server = Raytrace.site

app :: RTWebsocket.WSData -> Application
app wsdata = serve api (server wsdata)

-- TODO: refactor using https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html

-- 'Raytrace.Websocket.WSData' is mutable IPC object. (between Daemon and Server)
-- 'Raytrace.Daemon' receives input, runs raytrace assignment program, and send image file ID.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  wsdata <- RTWebsocket.initWSData
  _ <- forkFinally (RTDaemon.main wsdata) RTDaemon.onQuit
  run 8080 (app wsdata)
