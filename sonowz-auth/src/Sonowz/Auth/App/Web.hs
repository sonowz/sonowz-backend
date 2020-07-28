{-# LANGUAGE QuasiQuotes #-}
module Sonowz.Auth.App.Web
  ( api
  , server
  , runWithEffects
  , AuthAPI
  , Step1API
  , Step2API
  )
where

import Servant hiding (URI, uriPath)
import Network.HTTP.Client (Manager)
import Polysemy.Resource (resourceToIOFinal)
import URI.ByteString (URI, URIRef(uriPath))
import qualified Control.Exception.Safe as E

import Sonowz.Auth.Imports
import Sonowz.Auth.OAuth (FetchOAuthUser, fetchOAuthUserGoogle)
import Sonowz.Auth.Web.OAuth.Login
  ( GetOAuthRedirectURL
  , LoginWithOAuth
  , getOAuthRedirectURLHandlerRedirect
  , loginWithOAuthHandlerRedirect
  )
import Sonowz.Auth.Web.OAuth.Session (UserSession)
import Sonowz.Core.DB.Pool (DBEffects, DBConnPool)
import Sonowz.Core.Session.Effect (runSessionToIO)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv(..))

type AuthAPI = Step1API :<|> Step2API

type Step1API = "auth" :> (DefaultGetOAuthRedirectURL :<|> "google" :> GetOAuthRedirectURL)

type Step2API = "login" :> "google" :> LoginWithOAuth

type DefaultGetOAuthRedirectURL = GetOAuthRedirectURL

api :: Proxy AuthAPI
api = Proxy

-- https://github.com/lspitzner/brittany/issues/271
-- brittany-disable-next-binding
type AuthHandlerEffects
  = Reader WebAppEnv
  : Reader Manager
  : UserSession
  : Error ServerError
  : DBEffects

server :: Members AuthHandlerEffects r => WebAppEnv -> ServerT AuthAPI (Sem r)
server env = step1API (fetchSet env) :<|> step2API (fetchSet env)

runWithEffects :: WebAppEnv -> Manager -> DBConnPool -> Sem _ a -> Handler a
runWithEffects env manager dbPool (action :: Members AuthHandlerEffects r => Sem r a) =
  action
    & runReader env
    & runReader manager
    & runReader dbPool
    & runSessionToIO
    & stdEffToIO
    & resourceToIOFinal
    & errorToIOFinal
    & embedToFinal                -- Embed IO
    & (embed . liftIO . runFinal) -- Final IO
    & runM                        -- Embed Handler
    & logAndThrow
 where
  logAndThrow :: (E.MonadThrow m, MonadIO m) => m (Either ServerError a) -> m a
  logAndThrow action =
    action >>= either (\e -> (liftIO . logInfoIO . toText . displayException) e >> E.throw e) return


step1API :: Members AuthHandlerEffects r => FetchSetOAuthUser -> ServerT Step1API (Sem r)
step1API FetchSetOAuthUser {..} = handlerDefault :<|> handlerGoogle where
  handlerDefault = handlerGoogle -- Default OAuth is Google
  handlerGoogle  = getOAuthRedirectURLHandlerRedirect fetchGoogle

step2API :: Members AuthHandlerEffects r => FetchSetOAuthUser -> ServerT Step2API (Sem r)
step2API FetchSetOAuthUser {..} = handlerGoogle
  where handlerGoogle = loginWithOAuthHandlerRedirect fetchGoogle


data FetchSetOAuthUser = FetchSetOAuthUser
  { fetchGoogle :: FetchOAuthUser
  }

fetchSet :: WebAppEnv -> FetchSetOAuthUser
fetchSet WebAppEnv {..} = FetchSetOAuthUser 
  { fetchGoogle = fetchOAuthUserGoogle undefined ((eWebDomain :: URI) { uriPath = "/login/google" })
  }
