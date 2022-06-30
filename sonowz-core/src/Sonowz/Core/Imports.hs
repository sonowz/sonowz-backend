module Sonowz.Core.Imports
  ( module Relude,
    module Polysemy,
    module Polysemy.AtomicState,
    module Polysemy.Error,
    module Polysemy.Reader,
    module Polysemy.State,
    lengthText,
    unsafeLiftIO,
    type (<>),
  )
where

import Control.Monad.IO.Class qualified as IO
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
    -- This function is redefined in 'Sonowz.Core.StdEff.Effect'
    liftIO,
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

-- The original 'liftIO' function
unsafeLiftIO :: MonadIO m => IO a -> m a
unsafeLiftIO = IO.liftIO

-- Type-level list concatenation
type (<>) a b = Append a b
