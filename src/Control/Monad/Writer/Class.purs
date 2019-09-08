-- | This module defines the `MonadWriter` type class and its instances.

module Control.Monad.Indexed.Writer.Class where

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxMonad, ipure)
import Prelude

import Data.Tuple (Tuple(..))

-- | The `MonadTell w` type class represents those monads which support a
-- | monoidal accumulator of type `w`, where `tell` appends a value to the
-- | accumulator.
-- |
-- | An implementation is provided for `WriterT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Law:
-- |
-- | - `do { tell x ; tell y } = tell (x <> y)`
class IxMonad m <= IxMonadTell w m | m -> w where
  itell :: forall x. w -> m x x Unit

-- | An extension of the `MonadTell` class that introduces some operations on
-- | the accumulator:
-- |
-- | - `listen` modifies the result to include the changes to the accumulator.
-- | - `pass` applies the returned function to the accumulator.
-- |
-- | An implementation is provided for `WriterT`, and for other monad
-- | transformers defined in this library.
-- |
-- | Laws in addition to the `MonadTell` law:
-- |
-- | - `do { tell x ; tell y } = tell (x <> y)`
-- | - `listen (pure a) = pure (Tuple a mempty)`
-- | - `listen (writer a x) = tell x $> Tuple a x`
class IxMonadTell w m <= IxMonadWriter w m | m -> w where
  ilisten :: forall x a. m x x a -> m x x (Tuple a w)
  ipass :: forall x a. m x x (Tuple a (w -> w)) -> m x x a

-- | Projects a value from modifications made to the accumulator during an
-- | action.
ilistens :: forall w m x a b. IxMonadWriter w m => (w -> b) -> m x x a -> m x x (Tuple a b)
ilistens f m = Ix.do
  Tuple a w <- ilisten m
  ipure (Tuple a (f w))

-- | Modify the final accumulator value by applying a function.
icensor :: forall w m x a. IxMonadWriter w m => (w -> w) -> m x x a -> m x x a
icensor f m = ipass Ix.do
  a <- m
  ipure (Tuple a f)
