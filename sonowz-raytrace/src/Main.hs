module Main where

import           Relude
import           Control.Applicative
import           Control.Concurrent
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant
import qualified System.IO as IO

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

portP :: Parser Port
portP = argument (auto >>= checkPort) (metavar "PORT" <> value 80) where
  checkPort port = if 0 < port && port < 90000 then return port else empty

opts :: ParserInfo Port
opts = info (helper <*> portP) ( fullDesc <> progDesc "Raytrace backend server")

-- 'Raytrace.Websocket.WSData' is mutable IPC object. (between Daemon and Server)
-- 'Raytrace.Daemon' receives input, runs raytrace assignment program, and send image file ID.
main :: IO ()
main = do
  IO.hSetBuffering stdout IO.LineBuffering -- For debugging
  IO.hSetBuffering stderr IO.LineBuffering

  port <- execParser opts

  wsdata <- RTWebsocket.initWSData
  _ <- forkFinally (RTDaemon.main wsdata) RTDaemon.onQuit
  run port (app wsdata)
