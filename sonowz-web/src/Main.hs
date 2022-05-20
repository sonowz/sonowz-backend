module Main where

import Options.Applicative
import Sonowz.Web.Imports

import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Sonowz.Web.App as Web

import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port)
import Servant.Server (Handler, hoistServerWithContext, serveWithContext)
import Sonowz.Auth.OAuth (GoogleAppInfo(..))
import Sonowz.Auth.Web.OAuth.Types (OAuthContext, generateOAuthEnv, makeOAuthContext)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv(..), defaultWebAppEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering)


data Config = Config Port PGS.ConnectInfo GoogleAppInfo RootURI
type RootURI = Text

pGoogleAppInfo :: Parser GoogleAppInfo
pGoogleAppInfo = do
  appId     <- strOption (long "gappid")
  appSecret <- strOption (long "gappsecret")
  return GoogleAppInfo { .. }

pRootURI :: Parser RootURI
pRootURI = strOption (long "root" <> value "/api/" <> showDefault)

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pGoogleAppInfo <*> pRootURI

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Sonowz web API server")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo gAppInfo apiRoot) <- execParser opts
  dbPool     <- createConnPool pgConnectInfo
  tlsManager <- newTlsManager
  let webappEnv = defaultWebAppEnv { eWebAPIRoot = apiRoot, eWebPort = warpPort }
  oauthEnv <- generateOAuthEnv

  let
    waiApp = serveWithContext api context (hoistServerWithContext api contextProxy nt server)     where
    api          = Proxy :: Proxy Web.WebAPI
    contextProxy = Proxy :: Proxy OAuthContext
    context      = makeOAuthContext oauthEnv
    nt :: forall x . Sem _ x -> Handler x -- Natural Transformation from 'Sem r' to 'Handler' 
    nt     = Web.runWithEffects webappEnv oauthEnv tlsManager dbPool
    server = Web.webHandler webappEnv gAppInfo
  Warp.run warpPort waiApp
