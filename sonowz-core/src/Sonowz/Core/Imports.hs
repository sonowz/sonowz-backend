module Sonowz.Core.Imports
  ( module Relude,
    module Polysemy,
    module Polysemy.AtomicState,
    module Polysemy.Error,
    module Polysemy.Reader,
    module Polysemy.State,
    lengthText,
    type (<>),
  )
where

import Data.Text qualified as T
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Internal (Append)
import Polysemy.Reader
import Polysemy.State
import Relude hiding
  ( ExceptT (..),
    MonadReader (..),
    MonadState (..),
    Reader,
    ReaderT (..),
    State,
    StateT (..),
    asks,
    evalState,
    execState,
    fromException,
    gets,
    modify,
    modify',
    runReader,
    runState,
    withReader,
    withReaderT,
    withState,
  )

lengthText :: Text -> Int
lengthText = T.length

-- Type-level list concatenation
type (<>) a b = Append a b
