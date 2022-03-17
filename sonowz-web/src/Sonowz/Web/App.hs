module Sonowz.Web.App
  ( WebAPI
  , Auth.runWithEffects
  , webHandler
  ) where

import Servant
import Sonowz.Web.Imports

import qualified Sonowz.Auth.App.Web as Auth
import Sonowz.Auth.OAuth (GoogleAppInfo)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv)
import Sonowz.Web.Web.KVS (KVSAPI, kvsAPIHandler)
import Sonowz.Web.Web.User (UserAPI, userAPIHandler)


type WebAPI = Auth.AuthAPI :<|> ("kvs" :> KVSAPI) :<|> ("user" :> UserAPI)

webHandler
  :: Members Auth.AuthHandlerEffects r => WebAppEnv -> GoogleAppInfo -> ServerT WebAPI (Sem r)
webHandler env gAppInfo = Auth.server env gAppInfo :<|> kvsAPIHandler :<|> userAPIHandler
