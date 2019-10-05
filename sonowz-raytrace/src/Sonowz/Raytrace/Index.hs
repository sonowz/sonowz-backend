module Sonowz.Raytrace.Index where

import           Control.Applicative
import           Servant.Server

import qualified Sonowz.Raytrace.Types.API     as API
import qualified Sonowz.Raytrace.Websocket     as RTWebsocket

-- Snap entry point for URL 'raytrace/'
site :: RTWebsocket.WSData -> Server API.RaytraceWsAPI
site = RTWebsocket.wsServer
