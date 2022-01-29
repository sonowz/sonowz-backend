module Main where

import qualified Database.PostgreSQL.Simple as PGS
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Imports
import Sonowz.NewsCombinator.App.RuleWorker (runRuleWorker)
import Sonowz.NewsCombinator.Env (Env(..))
import System.IO (BufferMode(LineBuffering), hSetBuffering)


data Config = Config PGS.ConnectInfo Int

connectInfoP :: Parser PGS.ConnectInfo
connectInfoP = do
  let def = PGS.defaultConnectInfo
  connectHost     <- strOption (long "pghost" <> short 'h' <> value (PGS.connectHost def))
  connectPort     <- option auto (long "pgport" <> short 'P' <> value (PGS.connectPort def))
  connectUser     <- strOption (long "pguser" <> short 'u' <> value (PGS.connectUser def))
  connectPassword <- strOption (long "pgpasswd" <> short 'w')
  connectDatabase <- strOption (long "pgdatabase" <> short 'd' <> value (PGS.connectDatabase def))
  return PGS.ConnectInfo { .. }

workerIntervalP :: Parser Int
workerIntervalP = option
  auto
  (long "worker-interval" <> short 't' <> metavar "SECONDS" <> value 3600 <> help helpMsg)
  where helpMsg = "Time interval between worker runs (in seconds)"

configP :: Parser Config
configP = Config <$> connectInfoP <*> workerIntervalP

opts :: ParserInfo Config
opts = info (helper <*> configP) (fullDesc <> progDesc "News notification service")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config pgConnectInfo workerInterval) <- execParser opts
  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval

  runRuleWorker env
