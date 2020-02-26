module Sonowz.Raytrace.Web.Index where

import           Control.Applicative
import           Servant.Server

import qualified Sonowz.Raytrace.Types.API     as API
import qualified Sonowz.Raytrace.Web.Websocket as RTWebsocket

-- Snap entry point for URL 'raytrace/'
site :: RTWebsocket.WSData -> Server API.RaytraceWsAPI
site = RTWebsocket.wsServer
