module Sonowz.Noti.Web.HealthCheck
  ( HealthCheckAPI,
    healthCheckHandler,
  )
where

import Database.PostgreSQL.Simple (Connection, Only (..), query_)
import Polysemy.Fail (Fail, failToError)
import Servant
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Web.HealthCheck (HealthCheckAPI, healthCheckHandlerWithCheck)
import Sonowz.Noti.Imports

healthCheckHandler :: (Members (Error ServerError : DBEffects) r) => ServerT HealthCheckAPI (Sem r)
healthCheckHandler = healthCheckHandlerWithCheck (failToServerError $ withDBConn check)
  where
    failToServerError = failToError (\failMsg -> err503 {errBody = encodeUtf8 $ "Database connection failed: " <> failMsg})
    check :: (Members '[Fail, Error ServerError, Embed IO] r) => Connection -> Sem r Bool
    check = \conn -> do
      [Only x] <- webLiftIO (query_ conn "SELECT 1" :: IO [Only Int])
      pure (x == 1)
