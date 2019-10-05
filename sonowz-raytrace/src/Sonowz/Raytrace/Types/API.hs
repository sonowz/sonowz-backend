module Sonowz.Raytrace.Types.API where

import           Relude
import           Servant.API
import           Servant.API.WebSocket

type API = RaytraceWsAPI

type RaytraceWsAPI = "wait" :> WebSocket
