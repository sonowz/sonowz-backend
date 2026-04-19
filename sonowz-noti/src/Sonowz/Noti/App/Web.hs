module Sonowz.Noti.App.Web
  ( runServer,
  )
where

import Database.PostgreSQL.Simple (Connection, Only (..), query_)
import Polysemy.Fail (Fail, failToError)
import Polysemy.Resource (resourceToIOFinal)
import Servant
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Web.HealthCheck (HealthCheckAPI, healthCheckHandlerWithCheck)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..))
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports

api :: Proxy HealthCheckAPI
api = Proxy

runServer :: WebAppEnv -> Env -> IO ()
runServer webEnv env = runAppWithAccessLog (eWebPort webEnv) app
  where
    app = serve api $ hoistServer api (runWithEffects env) server

type ServerEffects = Error ServerError : DBEffects

server :: (Members ServerEffects r) => ServerT HealthCheckAPI (Sem r)
server = healthCheckHandlerWithDBConn

healthCheckHandlerWithDBConn :: (Members ServerEffects r) => ServerT HealthCheckAPI (Sem r)
healthCheckHandlerWithDBConn = healthCheckHandlerWithCheck (failToServerError $ withDBConn check)
  where
    failToServerError = failToError (\failMsg -> err503 {errBody = encodeUtf8 $ "Database connection failed: " <> failMsg})
    check :: (Members '[Fail, Error ServerError, Embed IO] r) => Connection -> Sem r Bool
    check = \conn -> do
      [Only x] <- webLiftIO (query_ conn "SELECT 1" :: IO [Only Int])
      pure (x == 1)

runWithEffects :: forall a. Env -> Sem _ a -> Handler a
runWithEffects env (action :: (Members ServerEffects r) => Sem r a) =
  action
    & runReader (envPgConnection env)
    & embedToFinal
    & resourceToIOFinal
    & stdEffToWebHandler
