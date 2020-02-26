module Sonowz.Raytrace.DaemonScript where

import Relude hiding (stdin, FilePath)
import Turtle
import UnliftIO.Exception

data ShellResult = ShellResult ExitCode StdOut StdErr
type StdOut = Text
type StdErr = Text

raytraceScript :: Int -> Text -> FilePath -> FilePath -> IO ShellResult
raytraceScript _id config raytracePath outputPath = catch raytraceScript' handler where
  raytraceScript' = do
    cd raytracePath
    writeTextFile "RTConf.h" config
    hasNixShell                               <- which "nix-shell"
    (buildExitCode, buildStdOut, buildStdErr) <- case hasNixShell of
      Just _  -> shellStrictWithErr "nix-shell --command runner" stdin -- Nix environment
      Nothing -> shellStrictWithErr "make && ./runner" stdin           -- Non-nix environment
    cp "out.png" (outputPath </> fromText (show _id <> ".png"))
    return (ShellResult buildExitCode buildStdOut buildStdErr)
  handler (_ :: SomeException) = return (ShellResult (ExitFailure 1) "" "Unexpected error")

example :: IO ()
example = void $ raytraceScript
  1
  exampleConfig
  "/home/sonowz/packages/raytrace"
  "/home/sonowz/data/www/graphics-demo/image/raytrace"
  where exampleConfig = ""
