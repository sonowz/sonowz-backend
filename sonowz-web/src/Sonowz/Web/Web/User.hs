module Sonowz.Web.Web.User
  ( UserAPI,
    userAPIHandler,
  )
where

import Data.Aeson (ToJSON)
import Servant
import Sonowz.Auth.OAuth.DB.Types (User (representation))
import Sonowz.Auth.Web.OAuth.Combinators (RequireAuth401, auth401)
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Web.Imports

type UserAPI = EchoAPI

type EchoAPI = RequireAuth401 :> Get '[JSON] UserResponse

newtype UserResponse = UserResponse {username :: Text} deriving (Generic)

instance ToJSON UserResponse

type UserAPIEffects =
  Error ServerError
    : DBEffects

userAPIHandler :: forall r. Members UserAPIEffects r => ServerT UserAPI (Sem r)
userAPIHandler auth = echoHandler
  where
    echoHandler = UserResponse . representation <$> auth401 auth
