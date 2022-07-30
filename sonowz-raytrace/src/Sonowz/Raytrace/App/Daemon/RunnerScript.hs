module Sonowz.Raytrace.App.Daemon.RunnerScript
  ( ShellResult (..),
    raytraceScript,
    example,
  )
where

import Control.Exception.Safe (onException)
import Relude
import System.Process
import System.Exit (ExitCode(..))
import Data.Text.IO (hGetContents)

data ShellResult = ShellResult ExitCode StdOut StdErr

type StdOut = Text

type StdErr = Text

raytraceScript :: Int -> Text -> FilePath -> FilePath -> IO ShellResult
raytraceScript _id config raytracePath outputPath = onException raytraceScript' unexpectedException
  where
    raytraceScript' = do
      writeFile (raytracePath <> "/RTConf.h") (toString config)
      let script = "cd " <> raytracePath <> "\n"
            <> "if which \"nix-shell\"; then\n"
            <> "  nix-shell --command runner\n"
            <> "else\n"
            <> "  make && ./runner\n"
            <> "fi\n"
            <> "cp out.png " <> (outputPath <> "/" <> show _id <> ".png")
      (_, Just hStdout, Just hStderr, _) <- createProcess (shell script) { std_out = CreatePipe, std_err = CreatePipe }
      ShellResult ExitSuccess <$> hGetContents hStdout <*>  hGetContents hStderr
    unexpectedException = return (ShellResult (ExitFailure 1) "" "Unexpected error")

example :: IO ()
example =
  void $
    raytraceScript
      1
      exampleConfig
      "/home/sonowz/packages/raytrace"
      "/home/sonowz/data/www/graphics-demo/image/raytrace"
  where
    exampleConfig = ""
