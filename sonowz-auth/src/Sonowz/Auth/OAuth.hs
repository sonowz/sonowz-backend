module Sonowz.Auth.OAuth
  ( module Sonowz.Auth.OAuth.Types
  , module Sonowz.Auth.OAuth.Google
  , fetchUserInfoFromExchangeToken
  )
where

import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (ExchangeToken, OAuth2Token(..), fetchAccessToken)
import qualified Control.Exception.Safe as E

import Sonowz.Auth.Imports
import Sonowz.Auth.OAuth.Types
import Sonowz.Auth.OAuth.Google

-- This function throws 'OAuthException'
fetchUserInfoFromExchangeToken :: Manager -> FetchOAuthUser -> ExchangeToken -> IO OAuthUser
fetchUserInfoFromExchangeToken tlsManager FetchOAuthUser {..} exchangeToken = do
  oauthResult <- throwOAuthError =<< fetchAccessToken tlsManager fetcherOAuthInfo exchangeToken
  let accessToken' = accessToken oauthResult
  throwGetUserError =<< fetcherGetOAuthUser tlsManager accessToken'
 where
  throwOAuthError   = either (E.throw . OAuthException . show) return
  throwGetUserError = either (E.throw . OAuthException) return
