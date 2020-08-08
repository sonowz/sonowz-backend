module Sonowz.Auth.Web.OAuth.Types
  ( OAuthContext
  , OAuthEnv
  , generateOAuthEnv
  , makeOAuthContext
  , LoginRedirectURL(..)
  )
where

import Servant hiding (URI)
import Servant.Auth.Server (CookieSettings(..), JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import URI.ByteString (URI)

import Sonowz.Auth.Imports

type OAuthContext = '[CookieSettings, JWTSettings]
type OAuthEnv = (CookieSettings, JWTSettings)

newtype LoginRedirectURL = LoginRedirectURL URI deriving (Show, Eq) via URI

-- TODO: store JWK in persistence
generateOAuthEnv :: IO OAuthEnv
generateOAuthEnv = do
  jwk <- generateKey
  return (defaultCookieSettings { cookieIsSecure = NotSecure, cookieXsrfSetting = Nothing }, defaultJWTSettings jwk)

makeOAuthContext :: OAuthEnv -> Context OAuthContext
makeOAuthContext (cookie, jwt) = cookie :. jwt :. EmptyContext
