module Sonowz.Auth.Web.OAuth.Types
  ( OAuthContext,
    OAuthEnv,
    generateOAuthEnv,
    makeOAuthContext,
    LoginRedirectURL (..),
  )
where

import Servant hiding (URI)
import Servant.Auth.Server
  ( CookieSettings (..),
    JWTSettings,
    SameSite (SameSiteStrict),
    XsrfCookieSettings (xsrfExcludeGet),
    defaultCookieSettings,
    defaultJWTSettings,
    defaultXsrfCookieSettings,
    generateKey,
  )
import Sonowz.Auth.Imports
import URI.ByteString (URI)

type OAuthContext = '[CookieSettings, JWTSettings]

type OAuthEnv = (CookieSettings, JWTSettings)

newtype LoginRedirectURL = LoginRedirectURL URI deriving (Show, Eq) via URI

-- TODO: store JWK in persistence
-- otherwise, all login sessions are invalidated when server restarts
generateOAuthEnv :: IO OAuthEnv
generateOAuthEnv = do
  jwk <- generateKey
  return
    ( defaultCookieSettings
        { cookieSameSite = SameSiteStrict,
          cookieXsrfSetting = Just xsrfSetting
        },
      defaultJWTSettings jwk
    )
  where
    xsrfSetting = defaultXsrfCookieSettings {xsrfExcludeGet = True}

makeOAuthContext :: OAuthEnv -> Context OAuthContext
makeOAuthContext (cookie, jwt) = cookie :. jwt :. EmptyContext
