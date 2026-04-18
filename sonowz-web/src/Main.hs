module Main where

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port)
import OptEnvConf
import Servant.Server (Handler, hoistServerWithContext, serveWithContext)
import Sonowz.Auth.OAuth (GoogleAppInfo (..))
import Sonowz.Auth.Web.OAuth.Types (OAuthContext, generateOAuthEnv, makeOAuthContext)
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)
import Sonowz.Web.App qualified as Web
import Sonowz.Web.Imports

data Config = Config Port PGS.ConnectInfo GoogleAppInfo RootURI

type RootURI = Text

pGoogleAppInfo :: Parser GoogleAppInfo
pGoogleAppInfo = do
  appId <- setting [help "Google App ID", reader str, long "gappid"]
  appSecret <- setting [help "Google App Secret", reader str, long "gappsecret"]
  return GoogleAppInfo {..}

pRootURI :: Parser RootURI
pRootURI = setting [help "Root URI", reader str, long "root", value "/api/"]

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pGoogleAppInfo <*> pRootURI

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo gAppInfo apiRoot) <-
    runParser (makeVersion []) "Sonowz web API server" pConfig
  dbPool <- createConnPool pgConnectInfo
  tlsManager <- newTlsManager
  let webappEnv = defaultWebAppEnv {eWebAPIRoot = apiRoot, eWebPort = warpPort}
  oauthEnv <- generateOAuthEnv

  let waiApp = serveWithContext api context (hoistServerWithContext api contextProxy nt server) where
      api = Proxy :: Proxy Web.WebAPI
      contextProxy = Proxy :: Proxy OAuthContext
      context = makeOAuthContext oauthEnv
      nt :: forall x. Sem _ x -> Handler x -- Natural Transformation from 'Sem r' to 'Handler'
      nt = Web.runWithEffects webappEnv oauthEnv tlsManager dbPool
      server = Web.webHandler webappEnv gAppInfo
  runAppWithAccessLog warpPort waiApp
