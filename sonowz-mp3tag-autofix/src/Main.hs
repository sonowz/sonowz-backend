module Main where

import Options.Applicative
import Sonowz.Mp3tagAutofix.Imports

import Colog.Core.Severity (Severity(..))
import Sonowz.Mp3tagAutofix.App (runMainFn)
import Sonowz.Mp3tagAutofix.Env (Env(..))
import System.IO (BufferMode(LineBuffering), hSetBuffering)


pEnv :: Parser Env
pEnv = do
  targetDir <- strArgument (metavar "dir")
  let
    niHelpMsg =
      "Run as noninteractive mode. WARNING: this will automatically update tags in the file!"
  nonInteractive <- switch (long "noninteractive" <> help niHelpMsg)
  let debugHelpMsg = "Print debug logs"
  debug <- switch (long "debug" <> help debugHelpMsg)
  return Env { .. }

opts :: ParserInfo Env
opts = info (helper <*> pEnv) (fullDesc <> progDesc "Mp3 tag autofix")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  env <- execParser opts
  setStdLogActionLevel (if debug env then Debug else Info)
  runMainFn env
