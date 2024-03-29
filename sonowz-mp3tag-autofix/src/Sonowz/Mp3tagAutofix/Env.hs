module Sonowz.Mp3tagAutofix.Env
  ( Env (..),
  )
where

import Sonowz.Mp3tagAutofix.Imports

data Env = Env
  { targetDir :: FilePath,
    nonInteractive :: Bool,
    debug :: Bool
  }
