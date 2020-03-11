module Main where

import Relude
import Control.Concurrent
import Network.Wai.Handler.Warp
import Options.Applicative
import Servant
import qualified System.IO as IO
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Types.Config (ProgramConfig(..))
import qualified Sonowz.Raytrace.Types.API as API
import qualified Sonowz.Raytrace.Daemon as RTDaemon
import qualified Sonowz.Raytrace.Web.Index as Raytrace
import qualified Sonowz.Raytrace.Web.Websocket as RTWebsocket

{-
new file structure
Main.hs
Sonowz.Raytrace
  Env.hs
  Monad
    MessageQueue
      Query.hs
    MessageQueue.hs --https://github.com/Holmusk/three-layer/blob/master/src/Lib/Effects/Session.hs
    Thread
      Blocking.hs
      Stoppable.hs -- can be gracefully killed (or just use `onExit`?)
    Thread.hs
    RunnerConfig.hs
  Web
    Types.hs
    MonadWebsocket.hs -- abstract Websocket interface to clean code `Websocket.hs`
    Thread
      Blocking.hs
      Stoppable.hs
    Thread.hs
    Websocket.hs
  Web.hs
  Daemon
    Thread
      Blocking.hs
    Thread.hs
    RunnerScript.hs
  Daemon.hs
    
-}

api :: Proxy API.API
api = Proxy

server :: RTWebsocket.WSData -> Server API.API
server = Raytrace.site

app :: RTWebsocket.WSData -> Application
app wsdata = serve api (server wsdata)

-- TODO: refactor using https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html

warpPortP :: Parser Port
warpPortP = option (auto >>= checkPort) (long "port" <> short 'p' <> value 80)
  where checkPort port = if 0 < port && port < 90000 then return port else empty

connectInfoP :: Parser PGS.ConnectInfo
connectInfoP = do
  let def = PGS.defaultConnectInfo
  host <- strOption (long "pghost" <> short 'h' <> value (connectHost def))
  port <- option auto (long "pgport" <> short 'P' <> value (connectPort def))
  user <- strOption (long "pguser" <> short 'u' <> value (connectUser def))
  password <- strOption (long "pgpasswd" <> short 'w' <> value (connectPassword def))
  database <- strOption (long "pgdatabase" <> short 'd' <> value (connectDatabase def))
  return PGS.ConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = user
    , connectPassword = password
    , connectDatabase = database
    }

configP :: Parser ProgramConfig
configP = do
  warpPort' <- warpPortP
  pgConnectInfo' <- connectInfoP
  return ProgramConfig { warpPort = warpPort, pgConnectInfo = pgConnectInfo' }

opts :: ParserInfo Port
opts = info (helper <*> configP) (fullDesc <> progDesc "Raytrace backend server")

-- 'Raytrace.Websocket.WSData' is mutable IPC object. (between Daemon and Server)
-- 'Raytrace.Daemon' receives input, runs raytrace assignment program, and send image file ID.
main :: IO ()
main = do
  IO.hSetBuffering stdout IO.LineBuffering -- For debugging
  IO.hSetBuffering stderr IO.LineBuffering

  port   <- execParser opts

  wsInitData <- RTDaemon.makeInitData
  _      <- forkFinally (RTDaemon.main wsdata) RTDaemon.onQuit
  run port (app wsdata)
