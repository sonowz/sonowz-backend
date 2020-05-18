module Sonowz.Raytrace.App.Web.Types where

import Servant.API
import Servant.API.WebSocket

type API = RaytraceWsAPI

type RaytraceWsAPI = "wait" :> WebSocket
