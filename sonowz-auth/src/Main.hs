module Main where

-- This module and runtime is used for OAuth test purpose
-- This module is not used in production

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port)
import OptEnvConf
import Servant (Proxy (..), (:<|>) (..))
import Servant.Server (Handler, hoistServerWithContext, serveWithContext)
import Sonowz.Auth.App.Test qualified as Test
import Sonowz.Auth.App.Web qualified as Web
import Sonowz.Auth.Imports hiding (Proxy)
import Sonowz.Auth.OAuth (GoogleAppInfo (..))
import Sonowz.Auth.Web.OAuth.Types (OAuthContext, generateOAuthEnv, makeOAuthContext)
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)

data Config = Config Port PGS.ConnectInfo GoogleAppInfo

pGoogleAppInfo :: Parser GoogleAppInfo
pGoogleAppInfo = do
  appId <- setting [help "Google App ID", reader str, long "gappid", env "GOOGLE_APP_ID"]
  appSecret <- setting [help "Google App Secret", reader str, long "gappsecret", env "GOOGLE_APP_SECRET"]
  return GoogleAppInfo {..}

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pGoogleAppInfo

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo gAppInfo) <-
    runParser (makeVersion []) "Authentication backend server (for development use!)" pConfig
  dbPool <- createConnPool pgConnectInfo
  tlsManager <- newTlsManager
  let webappEnv = defaultWebAppEnv {eWebAPIRoot = "/api/", eWebPort = warpPort}
  oauthEnv <- generateOAuthEnv

  let waiApp = serveWithContext api context (hoistServerWithContext api contextProxy nt server)
      api = Proxy :: Proxy (Web.AuthAPI :<|> Test.TestGetAPI)
      contextProxy = Proxy :: Proxy OAuthContext
      context = makeOAuthContext oauthEnv
      nt :: forall x. Sem _ x -> Handler x -- Natural Transformation from 'Sem r' to 'Handler'
      nt = Web.runWithEffects webappEnv oauthEnv tlsManager dbPool
      server = Web.server webappEnv gAppInfo :<|> Test.server
  runAppWithAccessLog warpPort waiApp
