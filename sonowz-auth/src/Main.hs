module Main where

-- This module and runtime is used for OAuth test purpose
-- This module is not used in production

import Database.PostgreSQL.Simple qualified as PGS
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Servant (Proxy (..), (:<|>) (..))
import Servant.Server (Handler, hoistServerWithContext, serveWithContext)
import Sonowz.Auth.App.Test qualified as Test
import Sonowz.Auth.App.Web qualified as Web
import Sonowz.Auth.Imports hiding (Proxy)
import Sonowz.Auth.OAuth (GoogleAppInfo (..))
import Sonowz.Auth.Web.OAuth.Types (OAuthContext, generateOAuthEnv, makeOAuthContext)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)

data Config = Config Port PGS.ConnectInfo GoogleAppInfo

pGoogleAppInfo :: Parser GoogleAppInfo
pGoogleAppInfo = do
  appId <- strOption (long "gappid")
  appSecret <- strOption (long "gappsecret")
  return GoogleAppInfo {..}

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pGoogleAppInfo

opts :: ParserInfo Config
opts =
  info
    (helper <*> pConfig)
    (fullDesc <> progDesc "Authentication backend server (for development use!)")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo gAppInfo) <- execParser opts
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
