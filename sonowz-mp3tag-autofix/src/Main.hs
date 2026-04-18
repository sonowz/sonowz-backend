module Main where

import Data.Version (makeVersion)
import OptEnvConf
import Sonowz.Mp3tagAutofix.App (runMainFn)
import Sonowz.Mp3tagAutofix.Env (Env (..))
import Sonowz.Mp3tagAutofix.Imports

pEnv :: Parser Env
pEnv = do
  targetDir <-
    setting
      [ help "Target directory",
        reader str,
        argument,
        metavar "dir"
      ]
  let niHelpMsg =
        "Run as noninteractive mode. WARNING: this will automatically update tags in the file!"
  nonInteractive <-
    setting
      [ help niHelpMsg,
        switch True,
        long "noninteractive",
        value False
      ]
  let debugHelpMsg = "Print debug logs"
  debug <-
    setting
      [ help debugHelpMsg,
        switch True,
        long "debug",
        value False
      ]
  return Env {..}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  env <- runParser (makeVersion []) "Mp3 tag autofix" pEnv
  setStdLogActionLevel (if debug env then Debug else Info)
  runMainFn env
