{-# LANGUAGE QuasiQuotes #-}
module Sonowz.Auth.OAuth.Google
  ( GoogleAppInfo(..)
  , fetchOAuthUserGoogle
  )
where

import Sonowz.Auth.Imports
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (OAuth2(..), AccessToken, authGetJSON)
import URI.ByteString (URI, Query(..), URIRef(..))
import URI.ByteString.QQ (uri)

import Sonowz.Auth.OAuth.Types (FetchOAuthUser(..), OAuthUser(..))

data GoogleAppInfo = GoogleAppInfo
  { appId :: Text
  , appSecret :: Text
  } deriving (Eq, Show)

data GoogleUserInfo = GoogleUserInfo
  { id :: Text
  , email :: Text
  , verifiedEmail :: Bool
  } deriving (Generic, Eq, Show, FromJSON, ToJSON)

urlOAuthRedirect = [uri|https://accounts.google.com/o/oauth2/v2/auth|]
urlOAuthAccessToken = [uri|https://oauth2.googleapis.com/token|]
urlGetUserInfo = [uri|https://www.googleapis.com/userinfo/v2/me|]

fetchOAuthUserGoogle :: GoogleAppInfo -> FetchOAuthUser
fetchOAuthUserGoogle appInfo = FetchOAuthUser
  { fetcherOAuthInfo      = oAuthInfoGoogle appInfo
  , fetcherOAuthClientURL = oAuthClientURLGoogle appInfo
  , fetcherGetOAuthUser   = getUserInfoGoogle
  }

oAuthInfoGoogle :: GoogleAppInfo -> OAuth2
oAuthInfoGoogle (GoogleAppInfo appId appSecret) = OAuth2
  { oauthClientId            = appId
  , oauthClientSecret        = Just appSecret
  , oauthCallback            = Nothing
  , oauthOAuthorizeEndpoint  = urlOAuthRedirect
  , oauthAccessTokenEndpoint = urlOAuthAccessToken
  }

oAuthClientURLGoogle :: GoogleAppInfo -> URI -> URI
oAuthClientURLGoogle (GoogleAppInfo appId _) redirectURL = url { uriQuery = queryParams } where
  url         = urlOAuthRedirect
  queryParams = Query
    [ ("client_id"    , encodeUtf8 appId)
    , ("scope", "https://www.googleapis.com/auth/userinfo.email")
    , ("response_type", "code")
    , ("access_type"  , "offline")
    , ("redirect_uri" , show redirectURL)
    ]

googleUserToOAuthUser :: GoogleUserInfo -> OAuthUser
googleUserToOAuthUser GoogleUserInfo {..} =
  OAuthUser { oauthUserProvider = "Google", oauthUserId = id, oauthUserRep = email }

getUserInfoGoogle :: Manager -> AccessToken -> IO (Either Text OAuthUser)
getUserInfoGoogle m at = authGetJSON m at urlGetUserInfo >>= \case
  Left  errormsg -> return . Left . decodeUtf8 $ errormsg
  Right userInfo -> return . Right . googleUserToOAuthUser $ userInfo
