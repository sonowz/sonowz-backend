module Sonowz.Auth.Web.OAuth.Login
  ( GetOAuthRedirectURL
  , getOAuthRedirectURLHandlerRedirect
  , getOAuthRedirectURLHandler
  , LoginWithOAuth
  , loginWithOAuthHandlerRedirect
  )
where

import Servant hiding (URI)
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (ExchangeToken(..))
import URI.ByteString (URI)
import Web.Cookie (SetCookie)

import Sonowz.Auth.Imports
import Sonowz.Auth.DB.Queries (insertOAuthUser)
import Sonowz.Auth.OAuth (FetchOAuthUser(..), fetchUserInfoFromExchangeToken)
import Sonowz.Auth.Web.OAuth.SessionRegister (UserSession, WithSessionHeader, registerUserSession)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv(..))

-- OAuth 1st step
type GetOAuthRedirectURL = Header "from" Referer :> Get '[] URI
-- OAuth 2nd step
type LoginWithOAuth
  = ReqParam "code" Text :> ReqParam "state" URI :> Get '[] (WithSessionHeader URI)

type ReqParam = QueryParam' '[Required, Strict]
newtype Referer = Referer ByteString deriving (Show, Eq)


type GetOAuthRedirectURLHandler
  =  forall r
   . Members '[Reader WebAppEnv, Error ServerError] r
  => FetchOAuthUser
  -> ServerT GetOAuthRedirectURL (Sem r)

-- Return as 301 response
getOAuthRedirectURLHandlerRedirect :: GetOAuthRedirectURLHandler
getOAuthRedirectURLHandlerRedirect fetch referer = do
  redirectURL <- getOAuthRedirectURLHandler fetch referer
  throw $ err301 { errHeaders = [("Location", show redirectURL)] }

-- Return URL as response body
getOAuthRedirectURLHandler :: GetOAuthRedirectURLHandler
getOAuthRedirectURLHandler FetchOAuthUser {..} referer = do
  WebAppEnv {..} <- ask
  let callbackURL = maybe (show eWebDomain) show referer
  let redirectURL = fetcherOAuthClientURL fetcherOAuthRegisterURL callbackURL
  return redirectURL

type LoginWithOAuthEffects = UserSession : Embed IO : Reader Manager : Error ServerError : DBEffects
type LoginWithOAuthHandler
  = forall r . Members LoginWithOAuthEffects r => FetchOAuthUser -> ServerT LoginWithOAuth (Sem r)

-- Return as 301 response
loginWithOAuthHandlerRedirect :: LoginWithOAuthHandler
loginWithOAuthHandlerRedirect f e r = throw301 =<< loginWithOAuth f e r where
  throw301 (setCookie, redirectURL) = throw'
    $ err301 { errHeaders = [("Location", show redirectURL), ("Set-Cookie", toHeader setCookie)] }

-- Return URL as response body (not OAuth spec)
{-
loginWithOAuthHandler :: LoginWithOAuthHandler
loginWithOAuthHandler f e r = uncurry addHeader <$> loginWithOAuth f e r
-}

loginWithOAuth
  :: Members LoginWithOAuthEffects r => FetchOAuthUser -> Text -> URI -> Sem r (SetCookie, URI)
loginWithOAuth fetch exchangeToken redirectURL = do
  tlsManager <- ask
  -- Do HTTP request to auth server
  oauthUser  <- liftIO
    $ fetchUserInfoFromExchangeToken tlsManager fetch (ExchangeToken exchangeToken)
  -- Insert to DB if new user, otherwise select from DB
  user      <- withDBConn (\conn -> liftIO $ insertOAuthUser conn oauthUser)
  -- Create new session
  setCookie <- registerUserSession user
  -- Return redirect URL
  return (setCookie, redirectURL)
