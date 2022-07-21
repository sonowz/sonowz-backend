module Sonowz.NewsCombinator.App.Web
  ( runServer,
  )
where

import Polysemy.Resource (resourceToIOFinal)
import Relude.Monad (ExceptT (ExceptT))
import Servant
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.DB.Utils (Uid (..))
import Sonowz.Core.Web.CRUD (CRUDAPI, crudHandlerFromDBQueries)
import Sonowz.Core.Web.Wai (runWithLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..))
import Sonowz.NewsCombinator.Env (Env (envPgConnection))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.DB.Queries (newsScrapRuleCRUD)
import Sonowz.NewsCombinator.Rule.DB.Types (NewsScrapRuleHask, NewsScrapRuleHaskW)

-- TODO: add auth
type NewsScrapRuleAPI = CRUDAPI NewsScrapRuleHask NewsScrapRuleHaskW Uid "rule"

api :: Proxy NewsScrapRuleAPI
api = Proxy

runServer :: WebAppEnv -> Env -> IO ()
runServer webEnv env = runWithLog (eWebPort webEnv) app
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
    & errorToIOFinal @ServerError
    & stdEffToIOFinal
    & runFinal @IO
    & logServerError
    & (Handler . ExceptT)
  where
    logServerError :: IO (Either ServerError a) -> IO (Either ServerError a)
    logServerError action = action >>= bitraverse (\e -> logInfoIO (show e) >> return e) return
