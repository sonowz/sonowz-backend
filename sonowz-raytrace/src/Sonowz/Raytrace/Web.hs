module Sonowz.Raytrace.Web
  ( server
  , api
  )
where

import Relude
import Servant
import qualified Network.WebSockets as WS

import Sonowz.Raytrace.Env (Env(..))
import Sonowz.Raytrace.Web.Types
import Sonowz.Raytrace.Web.Websocket (websocketHandler)

api :: Proxy RaytraceWsAPI
api = Proxy

server :: Env -> Server RaytraceWsAPI
server env = hoistServer api (runServantAppAsHandler env) (hoistConnToServantApp websocketHandler)

runServantAppAsHandler :: Env -> ServantApp a -> Handler a
runServantAppAsHandler env = liftIO . runServantAppAsIO env

runServantAppAsIO :: Env -> ServantApp a -> IO a
runServantAppAsIO Env {..} (ServantApp app) = runReaderT app ServantAppEnv { .. } where
  eWsConn = error "Websocket connection is not established"
  ePgConn = envPgConnection

hoistConnToServantApp :: ServantApp a -> WS.Connection -> ServantApp a
hoistConnToServantApp (ServantApp app) conn =
  ServantApp $ withReaderT (\env -> env { eWsConn = conn }) app
