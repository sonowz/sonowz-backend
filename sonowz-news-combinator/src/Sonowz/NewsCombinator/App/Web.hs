module Sonowz.NewsCombinator.App.Web
  ( runServer,
  )
where

import Polysemy.Resource (resourceToIOFinal)
import Servant
import Sonowz.Core.DB.Field (Uid (..))
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.StdEff.Effect (stdEffToWebHandler)
import Sonowz.Core.Web.CRUD (CRUDAPI, crudHandlerFromDBQueries)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..))
import Sonowz.NewsCombinator.Env (Env (envPgConnection))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.DB.Queries (newsScrapRuleCRUD)
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule)

-- TODO: add auth
type NewsScrapRuleAPI = CRUDAPI Uid NewsScrapRule NewsScrapRule "rule"

api :: Proxy NewsScrapRuleAPI
api = Proxy

runServer :: WebAppEnv -> Env -> IO ()
runServer webEnv env = runAppWithAccessLog (eWebPort webEnv) app
  where
    app = serve api $ hoistServer api (runWithEffects env) server

type ServerEffects = Error ServerError : DBEffects

server :: Members ServerEffects r => ServerT NewsScrapRuleAPI (Sem r)
server = crudHandlerFromDBQueries newsScrapRuleCRUD

runWithEffects :: forall a. Env -> Sem _ a -> Handler a
runWithEffects env (action :: Members ServerEffects r => Sem r a) =
  action
    & runReader (envPgConnection env)
    & embedToFinal
    & resourceToIOFinal
    & stdEffToWebHandler
