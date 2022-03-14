module Sonowz.Auth.App.Web
  ( AuthAPI
  , server
  , runWithEffects
  ) where

import Control.Monad.Except (MonadError)  -- 'mtl' package is used here
import Network.HTTP.Client (Manager)
import Polysemy.Resource (resourceToIOFinal)
import Servant hiding (URI, uriPath)
import URI.ByteString (URIRef(uriPath))

import Sonowz.Auth.Imports
import Sonowz.Auth.OAuth (FetchOAuthUser, GoogleAppInfo, fetchOAuthUserGoogle, identifierGoogle)
import Sonowz.Auth.Web.OAuth.Combinators (LoginRedirectURL(..))
import Sonowz.Auth.Web.OAuth.Login
  ( GetOAuthRedirectURL
  , LoginWithOAuth
  , getOAuthRedirectURLHandlerRedirect
  , loginWithOAuthHandlerRedirect
  )
import Sonowz.Auth.Web.OAuth.Types (OAuthEnv)
import Sonowz.Core.DB.Pool (DBConnPool, DBEffects)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv(..))

type AuthAPI = Step1API :<|> Step2API

type Step1API = "auth" :> (DefaultGetOAuthRedirectURL :<|> "google" :> GetOAuthRedirectURL)

type Step2API = "login" :> "google" :> LoginWithOAuth

type DefaultGetOAuthRedirectURL = GetOAuthRedirectURL

-- https://github.com/lspitzner/brittany/issues/271
-- brittany-disable-next-binding
type AuthHandlerEffects
  = Reader WebAppEnv
  : Reader LoginRedirectURL
  : Reader OAuthEnv
  : Reader Manager
  : Error ServerError
  : DBEffects

server :: Members AuthHandlerEffects r => WebAppEnv -> GoogleAppInfo -> ServerT AuthAPI (Sem r)
server env gAppInfo = step1API fetchSet' :<|> step2API fetchSet'
  where fetchSet' = fetchSet env gAppInfo

runWithEffects :: forall a . WebAppEnv -> OAuthEnv -> Manager -> DBConnPool -> Sem _ a -> Handler a
runWithEffects env oauthEnv manager dbPool (action :: Members AuthHandlerEffects r => Sem r a) =
  action
    & runReader env
    & runReader (loginRedirectURL env)
    & runReader oauthEnv
    & runReader manager
    & runReader dbPool
    & stdEffToIO
    & resourceToIOFinal
    & errorToIOFinal
    & embedToFinal                -- Embed IO
    & (embed . liftIO . runFinal) -- Final IO
    & runM                        -- Embed Handler
    & logAndThrow
 where
  logAndThrow :: (MonadError ServerError m, MonadIO m) => m (Either ServerError a) -> m a
  logAndThrow action =
    action
      >>= either (\e -> (liftIO . logInfoIO . toText . displayException) e >> throwError e) return

step1API :: Members AuthHandlerEffects r => FetchSetOAuthUser -> ServerT Step1API (Sem r)
step1API FetchSetOAuthUser {..} = handlerDefault :<|> handlerGoogle where
  handlerDefault = handlerGoogle -- Default OAuth is Google
  handlerGoogle  = getOAuthRedirectURLHandlerRedirect fetchGoogle

step2API :: Members AuthHandlerEffects r => FetchSetOAuthUser -> ServerT Step2API (Sem r)
step2API FetchSetOAuthUser {..} = handlerGoogle
  where handlerGoogle = loginWithOAuthHandlerRedirect fetchGoogle

-- This would be "https://sonowz.me/api/login/google"
loginRedirectURL :: WebAppEnv -> LoginRedirectURL
loginRedirectURL WebAppEnv {..} = LoginRedirectURL
  (eWebDomain { uriPath = encodeUtf8 (eWebAPIRoot <> "login/" <> identifierGoogle) })


data FetchSetOAuthUser = FetchSetOAuthUser
  { fetchGoogle :: FetchOAuthUser
  }

fetchSet :: WebAppEnv -> GoogleAppInfo -> FetchSetOAuthUser
fetchSet WebAppEnv {..} gAppInfo = FetchSetOAuthUser
  { fetchGoogle = fetchOAuthUserGoogle
    gAppInfo
    (eWebDomain { uriPath = encodeUtf8 (eWebAPIRoot <> "login/" <> identifierGoogle) })
  }
