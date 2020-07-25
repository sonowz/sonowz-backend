module Sonowz.Auth.Web.OAuth.SessionRegister
  ( UserSession
  , SessionHeader
  , WithSessionHeader
  , registerUserSession
  )
where

import Servant
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict)

import Sonowz.Auth.Imports
import Sonowz.Auth.DB.Types (UserInfo)
import Sonowz.Auth.Web.OAuth.Session (UserSession)
import Sonowz.Core.Session.Effect (ExpirySec, newSession)


type SessionHeader = Header "Set-Cookie" SetCookie
type WithSessionHeader = Headers '[SessionHeader]

defaultExpirySec = 14400 :: ExpirySec -- 4 Hours

registerUserSession :: Member UserSession r => UserInfo -> Sem r SetCookie
registerUserSession user = do
  sessionKey <- newSession defaultExpirySec user
  return $ defaultSetCookie
    { setCookieName     = "session"
    , setCookieValue    = encodeUtf8 sessionKey
    , setCookiePath     = Just "/"
    , setCookieSameSite = Just sameSiteStrict
    }
