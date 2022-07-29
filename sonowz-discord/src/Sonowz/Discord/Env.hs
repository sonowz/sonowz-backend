module Sonowz.Discord.Env
  ( Env (..),
  )
where

import Discord.Types (UserId)
import Sonowz.Discord.Imports

data Env = Env
  { sonowzId :: UserId
  }
