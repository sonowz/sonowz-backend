
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Sonowz.Auth.Imports
import Network.HTTP.Client.TLS (newTlsManager)
import Network.OAuth.OAuth2 (OAuth2(..), ExchangeToken(..), fetchAccessToken, AccessToken(..), authGetJSON)
import Data.Aeson
import URI.ByteString.QQ

main :: IO ()
main = pass