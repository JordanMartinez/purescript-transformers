-- | This module defines the `IxMonadState` type class and its instances.

module Control.Monad.Indexed.State.Class where

import Data.Indexed (Indexed(..))
import Control.Monad.Indexed (class IxMonad, imap)
import Prelude (class Monad, Unit, unit)

import Data.Tuple (Tuple(..))

-- | The `IxMonadState s` type class represents those monads which support a single piece of mutable
-- | istate of type `s`.
-- |
-- | - `istate f` updates the istate using the function `f`.
-- |
-- | An implementation is provided for `StateT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - `do { get ; get } = get`
-- | - `do { put x ; put y } = put y`
-- | - `do { put x ; get } = put x $> x`
-- | - `do { s <- get ; put s } = pure unit`
-- |
class IxMonad m <= IxMonadState s m | m -> s where
  istate :: forall x a. (s -> (Tuple a s)) -> m x x a

-- | Get the current state.
iget :: forall m s x. IxMonadState s m => m x x s
iget = istate \s -> Tuple s s

-- | Get a value which depends on the current state.
igets :: forall s m x a. IxMonadState s m => (s -> a) -> m x x a
igets f = istate \s -> Tuple (f s) s

-- | Set the state.
iput :: forall m s x. IxMonadState s m => s -> m x x Unit
iput s = istate \_ -> Tuple unit s

-- | Modify the state by applying a function to the current state. The returned
-- | value is the new istate value.
imodify :: forall s m x. IxMonadState s m => (s -> s) -> m x x s
imodify f = istate \s -> let s' = f s in Tuple s' s'

imodify_ :: forall s m x. IxMonadState s m => (s -> s) -> m x x Unit
imodify_ f = istate \s -> Tuple unit (f s)
