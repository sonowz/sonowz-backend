{-# LANGUAGE QuasiQuotes #-}

module Sonowz.Auth.OAuth.Google
  ( GoogleAppInfo (..),
    fetchOAuthUserGoogle,
    identifierGoogle,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (AccessToken, OAuth2 (..), authGetJSON)
import Sonowz.Auth.Imports
import Sonowz.Auth.OAuth.Types (FetchOAuthUser (..), OAuthUser (..))
import URI.ByteString (Query (..), URI, URIRef (..), serializeURIRef')
import URI.ByteString.QQ (uri)

data GoogleAppInfo = GoogleAppInfo
  { appId :: Text,
    appSecret :: Text
  }
  deriving (Eq, Show)

data GoogleUserInfo = GoogleUserInfo
  { id :: Text,
    email :: Text,
    verifiedEmail :: Bool
  }
  deriving (Generic, Eq, Show)

instance FromJSON GoogleUserInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON GoogleUserInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

identifierGoogle = "google" :: Text

urlOAuthRedirect = [uri|https://accounts.google.com/o/oauth2/v2/auth|]

urlOAuthAccessToken = [uri|https://oauth2.googleapis.com/token|]

urlGetUserInfo = [uri|https://www.googleapis.com/userinfo/v2/me|]

fetchOAuthUserGoogle :: GoogleAppInfo -> URI -> FetchOAuthUser
fetchOAuthUserGoogle appInfo registerURL =
  FetchOAuthUser
    { fetcherOAuthInfo = oAuthInfoGoogle appInfo registerURL,
      fetcherOAuthClientURL = oAuthClientURLGoogle appInfo,
      fetcherGetOAuthUser = getUserInfoGoogle,
      fetcherOAuthRegisterURL = registerURL
    }

oAuthInfoGoogle :: GoogleAppInfo -> URI -> OAuth2
oAuthInfoGoogle (GoogleAppInfo appId appSecret) registerURL =
  OAuth2
    { oauthClientId = appId,
      oauthClientSecret = Just appSecret,
      oauthCallback = Just registerURL, -- Google requires this URL to be same as serverside register URL
      oauthOAuthorizeEndpoint = urlOAuthRedirect,
      oauthAccessTokenEndpoint = urlOAuthAccessToken
    }

oAuthClientURLGoogle :: GoogleAppInfo -> URI -> Text -> URI
oAuthClientURLGoogle (GoogleAppInfo appId _) redirectURL state = url {uriQuery = queryParams}
  where
    url = urlOAuthRedirect
    queryParams =
      Query
        [ ("client_id", encodeUtf8 appId),
          ("scope", "https://www.googleapis.com/auth/userinfo.email"),
          ("response_type", "code"),
          ("access_type", "offline"),
          ("redirect_uri", serializeURIRef' redirectURL),
          ("state", encodeUtf8 state)
        ]

googleUserToOAuthUser :: GoogleUserInfo -> OAuthUser
googleUserToOAuthUser GoogleUserInfo {..} =
  OAuthUser {oauthUserProvider = "Google", oauthUserId = id, oauthUserRep = email}

getUserInfoGoogle :: Manager -> AccessToken -> IO (Either Text OAuthUser)
getUserInfoGoogle m at =
  authGetJSON m at urlGetUserInfo >>= \case
    Left errormsg -> return . Left . decodeUtf8 $ errormsg
    Right userInfo -> return . Right . googleUserToOAuthUser $ userInfo
