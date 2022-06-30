{-# LANGUAGE QuasiQuotes #-}

module Sonowz.Core.Web.WebAppEnv
  ( WebAppEnv (..),
    defaultWebAppEnv,
  )
where

import Network.Wai.Handler.Warp (Port)
import Sonowz.Core.Imports
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)

data WebAppEnv = WebAppEnv
  { eWebDomain :: URI,
    eWebAPIRoot :: Text, -- example: "/api"
    eWebPort :: Port
  }

defaultWebAppEnv :: WebAppEnv
defaultWebAppEnv =
  WebAppEnv {eWebDomain = [uri|https://sonowz.me|], eWebAPIRoot = "", eWebPort = 80}
