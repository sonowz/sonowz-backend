module Sonowz.Auth.Web.OAuth.Logout
  ( LogoutOAuth,
    logoutOAuthHandler,
  )
where

import Servant
import Servant.Auth.Server (SetCookie, clearSession)
import Sonowz.Auth.Imports
import Sonowz.Auth.Web.OAuth.Combinators (RequireAuth401)
import Sonowz.Auth.Web.OAuth.Types (OAuthEnv)
import Sonowz.Core.DB.Pool (DBEffects)

type LogoutOAuth = RequireAuth401 :> Post '[PlainText] (LogoutHeaders NoContent)

type LogoutHeaders = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

type LogoutOAuthEffects =
  Reader OAuthEnv
    : DBEffects

logoutOAuthHandler :: forall r. Members LogoutOAuthEffects r => ServerT LogoutOAuth (Sem r)
logoutOAuthHandler _ = do
  (cookieSettings, _) <- ask
  return $ clearSession cookieSettings NoContent
