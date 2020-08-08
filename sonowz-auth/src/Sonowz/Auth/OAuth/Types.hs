module Sonowz.Auth.OAuth.Types
  ( FetchOAuthUser(..)
  , OAuthUser(..)
  , OAuthException(..)
  )
where

import Sonowz.Auth.Imports
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (OAuth2, AccessToken)
import URI.ByteString (URI)
import qualified Text.Show as S


data OAuthException = OAuthException Text deriving (Show, Exception)

data FetchOAuthUser = FetchOAuthUser
  { fetcherOAuthClientURL :: URI -> Text -> URI -- (Redirect URL, State) as parameter
  , fetcherOAuthInfo :: OAuth2
  , fetcherOAuthRegisterURL :: URI -- Register URL in this 'sonowz-auth' service
  , fetcherGetOAuthUser :: Manager -> AccessToken -> IO (Either Text OAuthUser)
  }

data OAuthUser = OAuthUser
  { oauthUserProvider :: Text
  , oauthUserId :: Text -- This must be unique within same provider
  , oauthUserRep :: Text -- This is representation of user
  } deriving (Eq)

instance S.Show OAuthUser where
  show (OAuthUser _ _ rep) = toString rep
