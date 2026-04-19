module Sonowz.Core.Web.HealthCheck
  ( HealthCheckAPI,
    HealthStatus (..),
    healthCheckHandlerSimple,
    healthCheckHandlerWithCheck,
  )
where

import Data.Aeson (ToJSON)
import Servant
import Sonowz.Core.Imports

newtype HealthStatus = HealthStatus {status :: Text}
  deriving (Generic)
  deriving anyclass (ToJSON)

type HealthCheckAPI = "health" :> Get '[JSON] HealthStatus

healthCheckHandlerSimple :: ServerT HealthCheckAPI (Sem r)
healthCheckHandlerSimple = pure $ HealthStatus "UP"

healthCheckHandlerWithCheck :: Sem r Bool -> ServerT HealthCheckAPI (Sem r)
healthCheckHandlerWithCheck = fmap (HealthStatus . \result -> if result then "UP" else "DOWN")