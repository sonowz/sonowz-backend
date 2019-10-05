{-# LANGUAGE OverloadedStrings #-}
module Raytrace.Index where

import           Control.Applicative
import           Snap.Core
import qualified Network.WebSockets.Snap as WS

import qualified Raytrace.Websocket as RTWebsocket

-- Snap entry point for URL 'raytrace/'
site :: RTWebsocket.WSData -> Snap ()
site wsdata =
  -- Image will be served by NGINX
  --ifTop (method GET rtServe) <|>
  dir "wait" $ WS.runWebSocketsSnap $ RTWebsocket.websocketApp wsdata
