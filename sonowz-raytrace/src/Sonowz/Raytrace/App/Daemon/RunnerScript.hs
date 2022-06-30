module Sonowz.Raytrace.App.Daemon.RunnerScript
  ( ShellResult (..),
    raytraceScript,
    example,
  )
where

import Control.Exception.Safe (onException)
import Relude hiding (FilePath, stdin)
import Turtle

data ShellResult = ShellResult ExitCode StdOut StdErr

type StdOut = Text

type StdErr = Text

raytraceScript :: Int -> Text -> FilePath -> FilePath -> IO ShellResult
raytraceScript _id config raytracePath outputPath = onException raytraceScript' unexpectedException
  where
    raytraceScript' = do
      cd raytracePath
      writeTextFile "RTConf.h" config
      hasNixShell <- which "nix-shell"
      (buildExitCode, buildStdOut, buildStdErr) <- case hasNixShell of
        Just _ -> shellStrictWithErr "nix-shell --command runner" stdin -- Nix environment
        Nothing -> shellStrictWithErr "make && ./runner" stdin -- Non-nix environment
      cp "out.png" (outputPath </> fromText (show _id <> ".png"))
      return (ShellResult buildExitCode buildStdOut buildStdErr)
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
