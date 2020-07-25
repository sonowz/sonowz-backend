module Sonowz.Auth.Web.OAuth.Session
  ( UserSession
  , LoginRedirectURL(..)
  , RequireSession301
  , RequireSession401
  , MaybeSession
  , authSessionContext
  , authSessionContext2
  )
where

import Servant
import Servant.Server.Experimental.Auth (AuthServerData)
import Network.Wai (Request, requestHeaders)
import Web.Cookie (parseCookies)

import Sonowz.Auth.Imports
import Sonowz.Auth.DB.Types (UserInfo)
import Sonowz.Core.Session.Effect (Session, SessionKey, getSession)


-- Servant combinators with regard to user session --

type UserSession = Session UserInfo
newtype LoginRedirectURL = LoginRedirectURL URI deriving (Show, Eq) via URI
newtype UserInfo' a = UserInfo' UserInfo -- Phantom type to discriminate between 301 and 401

-- Returns 301 Redirect to default OAuth login URL when no session
type RequireSession301 = AuthProtect "require-session-301"
type instance AuthServerData (AuthProtect "require-session-301") = UserInfo' "301"
type RS301ContextHandler r = Request -> Sem r (UserInfo' "301")

-- Returns 401 Unauthorized when no session
type RequireSession401 = AuthProtect "require-session-401"
type instance AuthServerData (AuthProtect "require-session-401") = UserInfo' "401"
type RS401ContextHandler r = Request -> Sem r (UserInfo' "401")

-- Pass Nothing when no session
type MaybeSession = AuthProtect "maybe-session"
type instance AuthServerData (AuthProtect "maybe-session") = Maybe UserInfo
type MSContextHandler r = Request -> Sem r (Maybe UserInfo)

authSessionContext
  :: Members '[UserSession, Error ServerError] r
  => Context '[RS401ContextHandler r, MSContextHandler r]
authSessionContext = requireSession :. maybeSession :. EmptyContext

authSessionContext2
  :: Members '[Reader LoginRedirectURL, UserSession, Error ServerError] r
  => Context '[RS301ContextHandler r, RS401ContextHandler r, MSContextHandler r]
authSessionContext2 = requireSessionRedirect :. requireSession :. maybeSession :. EmptyContext

requireSession :: Members '[UserSession, Error ServerError] r => RS401ContextHandler r
requireSession req = do
  sessionKey :: SessionKey <- getSessionKey
  maybeUser                <- getSession sessionKey
  UserInfo' <$> maybe (throw401 "Missing session in sessionStore") return maybeUser
 where
  lookupKVList :: forall a b . Eq a => a -> [(a, b)] -> Maybe b
  lookupKVList name = fmap snd . find (\(headerName, _) -> headerName == name)
  throw401 :: Member (Error ServerError) r => LByteString -> Sem r a
  throw401 msg = throw $ err401 { errBody = msg }
  getSessionKey = either throw401 return $ do
    cookies    <- maybeToRight "Missing cookie header" $ lookupKVList "cookie" $ requestHeaders req
    sessionKey <- maybeToRight "Missing token in cookie" $ lookupKVList "session" $ parseCookies
      cookies
    first (const "ByteString decode failed") (decodeUtf8' sessionKey)

requireSessionRedirect
  :: Members '[Reader LoginRedirectURL, UserSession, Error ServerError] r => RS301ContextHandler r
requireSessionRedirect req = do
  loginRedirectURL <- ask
  catch
    (coerce <$> requireSession req)
    (\(_ :: ServerError) -> throw err401 { errHeaders = [("Location", show loginRedirectURL)] })

maybeSession :: Members '[UserSession, Error ServerError] r => MSContextHandler r
maybeSession req =
  catch (Just . coerce <$> requireSession req) (\(_ :: ServerError) -> return Nothing)

