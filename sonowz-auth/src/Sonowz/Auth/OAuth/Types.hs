module Sonowz.Auth.OAuth.Types
  ( FetchOAuthUser (..),
    OAuthUser (..),
    OAuthException (..),
    UserInfo (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (AccessToken, OAuth2)
import Servant.Auth.Server (FromJWT, ToJWT)
import Sonowz.Auth.Imports
import Sonowz.Core.DB.Field (Uid)
import Text.Show qualified as S
import URI.ByteString (URI)

newtype OAuthException = OAuthException Text
  deriving (Show)
  deriving anyclass (Exception)

data FetchOAuthUser = FetchOAuthUser
  { fetcherOAuthClientURL :: URI -> Text -> URI, -- (Redirect URL, State) as parameter
    fetcherOAuthInfo :: OAuth2,
    fetcherOAuthRegisterURL :: URI, -- Register URL in this 'sonowz-auth' service
    fetcherGetOAuthUser :: Manager -> AccessToken -> IO (Either Text OAuthUser)
  }

data OAuthUser = OAuthUser
  { oauthUserProvider :: Text,
    oauthUserId :: Text, -- This must be unique within same provider
    oauthUserRep :: Text -- This is representation of user
  }
  deriving (Eq)

instance S.Show OAuthUser where
  show (OAuthUser _ _ rep) = toString rep

data UserInfo = UserInfo
  { uid :: Uid,
    oauthProvider :: Text,
    oauthId :: Text,
    representation :: Text,
    createdTime :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON UserInfo

instance ToJWT UserInfo

instance FromJSON UserInfo

instance FromJWT UserInfo
