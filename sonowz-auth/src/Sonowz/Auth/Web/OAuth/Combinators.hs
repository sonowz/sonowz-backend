{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sonowz.Auth.Web.OAuth.Combinators
  ( RequireAuth301,
    RequireAuth401,
    RequireAuthMaybe,
    auth301,
    auth401,
    authMaybe,
    authMaybe',
    LoginRedirectURL (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Servant
import Servant.Auth.Server (Auth, AuthResult (..), Cookie, FromJWT, JWT, ToJWT)
import Sonowz.Auth.Imports
import Sonowz.Auth.OAuth.Types (UserInfo)
import Sonowz.Auth.Web.OAuth.Types (LoginRedirectURL (..))
import URI.ByteString (serializeURIRef')

type RequireAuth301 = Auth '[JWT, Cookie] (AuthUserInfo "301")

type RequireAuth401 = Auth '[JWT, Cookie] (AuthUserInfo "401")

type RequireAuthMaybe = Auth '[JWT, Cookie] (AuthUserInfo "maybe")

-- Phantom type to distinguish between response types
newtype AuthUserInfo a = AuthUserInfo {getUserInfo :: UserInfo} deriving (Show, FromJSON, ToJSON, FromJWT, ToJWT) via UserInfo

auth301 ::
  Members '[Reader LoginRedirectURL, Error ServerError] r =>
  AuthResult (AuthUserInfo "301") ->
  Sem r UserInfo
auth301 authResult = do
  loginRedirectURL <- coerce <$> ask
  authEither
    (return . getUserInfo)
    (\a -> throw (err301 {errBody = "Authentication failed with: " <> show a, errHeaders = [("Location", serializeURIRef' loginRedirectURL)]}))
    authResult

auth401 :: Member (Error ServerError) r => AuthResult (AuthUserInfo "401") -> Sem r UserInfo
auth401 = authEither (return . getUserInfo) $
  \a -> throw (err401 {errBody = "Authentication failed with: " <> show a})

authMaybe :: AuthResult (AuthUserInfo "maybe") -> Sem r (Maybe UserInfo)
authMaybe = return . authMaybe'

authMaybe' :: AuthResult (AuthUserInfo "maybe") -> Maybe UserInfo
authMaybe' = authEither (Just . getUserInfo) (const Nothing)

authEither :: (a -> c) -> (AuthResult a -> c) -> AuthResult a -> c
authEither f _ (Authenticated val) = f val
authEither _ g auth = g auth
