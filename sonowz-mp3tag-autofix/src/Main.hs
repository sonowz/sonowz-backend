module Main where

import Sonowz.Mp3tagAutofix.App (runMainFn)
import Sonowz.Mp3tagAutofix.Env (Env(..))
import Sonowz.Mp3tagAutofix.Imports
import System.IO (BufferMode(LineBuffering), hSetBuffering)



main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  runMainFn $ Env "./test" False
