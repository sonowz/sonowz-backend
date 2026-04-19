module Sonowz.Noti.App.Web
  ( runServer,
  )
where

import Polysemy.Resource (resourceToIOFinal)
import Servant
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.MessageQueue.Effect (MessageQueue)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..))
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports
import Sonowz.Noti.MessageQueue.DB (runMQueueDBNoti)
import Sonowz.Noti.Notification.Types (Notification (..))
import Sonowz.Noti.Web.HealthCheck (HealthCheckAPI, healthCheckHandler)
import Sonowz.Noti.Web.Notification (NotificationAPI, notificationHandler)

type API = HealthCheckAPI :<|> NotificationAPI

api :: Proxy API
api = Proxy

runServer :: WebAppEnv -> Env -> IO ()
runServer webEnv env = runAppWithAccessLog (eWebPort webEnv) app
  where
    app = serve api $ hoistServer api (runWithEffects env) server

type ServerEffects = Error ServerError : MessageQueue Notification : DBEffects

server :: (Members ServerEffects r) => ServerT API (Sem r)
server = healthCheckHandler :<|> notificationHandler

runWithEffects :: forall a. Env -> Sem _ a -> Handler a
runWithEffects env (action :: (Members ServerEffects r) => Sem r a) =
  action
    & runMQueueDBNoti
    & runReader (envPgConnection env)
    & embedToFinal
    & resourceToIOFinal
    & stdEffToWebHandler
