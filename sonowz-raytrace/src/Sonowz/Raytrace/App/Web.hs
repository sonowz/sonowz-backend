module Sonowz.Raytrace.App.Web
  ( server,
    api,
  )
where

import Servant
import Sonowz.Raytrace.App.Web.Types (RaytraceWsAPI)
import Sonowz.Raytrace.App.Web.Websocket (websocketHandler)
import Sonowz.Raytrace.Env (Env (..))
import Sonowz.Raytrace.Imports

api :: Proxy RaytraceWsAPI
api = Proxy

server :: Env -> Server RaytraceWsAPI
server Env {..} = liftIO . websocketHandler envPgConnection
