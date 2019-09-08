-- | This module defines the `MonadReader` type class and its instances.

module Control.Monad.Indexed.Reader.Class where

import Data.Indexed (Indexed(..))
import Control.Monad.Indexed (class IxMonad, imap)
import Prelude

-- | The `MonadAsk` type class represents those monads which support a global
-- | context that can be provided via the `ask` function.
-- |
-- | An implementation is provided for `ReaderT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Law:
-- |
-- | - `do { ask ; ask } = ask`
class IxMonad m <= IxMonadAsk r m | m -> r where
  iask :: forall x. m x x r

instance ixMonadAskFun :: IxMonadAsk r (Indexed ((->) r)) where
  iask = Indexed identity

-- | Projects a value from the global context in a `MonadAsk`.
iasks :: forall r m x a. IxMonadAsk r m => (r -> a) -> m x x a
iasks f = imap f iask

-- | An extension of the `MonadAsk` class that introduces a function `local f x`
-- | that allows the value of the local context to be modified for the duration
-- | of the execution of action `x`.
-- |
-- | An implementation is provided for `ReaderT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Laws in addition to the `MonadAsk` law:
-- |
-- | - `local f ask = f <$> ask`
-- | - `local _ (pure a) = pure a`
-- | - `local f (do { a <- x ; y }) = do { a <- local f x ; local f y }`
class IxMonadAsk r m <= IxMonadReader r m | m -> r where
  ilocal :: forall x a. (r -> r) -> m x x a -> m x x a

instance ixMonadReaderFun :: IxMonadReader r (Indexed ((->) r)) where
  ilocal f (Indexed m) = Indexed \a -> m (f a)
