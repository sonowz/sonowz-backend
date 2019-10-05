{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.IO
import           Control.Applicative
import           Control.Concurrent
import           Snap.Core
import           Snap.Http.Server
import qualified Snap.Internal.Http.Server.Config as Server.Config

import qualified Raytrace.Daemon
import qualified Raytrace.Index as Raytrace
import qualified Raytrace.Websocket

config :: Server.Config.Config Snap a
config = 
  -- setHostname "TODO" .
  setLocale "ko_KR.UTF-8" .
  setPort 3333 .
  setDefaultTimeout 2 $ emptyConfig

-- 'Raytrace.Websocket.WSData' is mutable IPC object. (between Daemon and Server)
-- 'Raytrace.Daemon' receives input, runs raytrace assignment program, and send image file ID.
-- 'simpleHttpServe' serves HTTP request.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  wsdata <- Raytrace.Websocket.initWSData
  _ <- forkFinally (Raytrace.Daemon.main wsdata) Raytrace.Daemon.onQuit
  simpleHttpServe config (site wsdata)

site :: Raytrace.Websocket.WSData -> Snap ()
site wsdata =
  ifTop (finishWith $ setResponseCode 404 emptyResponse) <|>
  route
    [ ("raytrace", Raytrace.site wsdata)
    ]
