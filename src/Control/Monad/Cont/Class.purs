-- | This module defines the `MonadCont` type class and its instances.

module Control.Monad.Indexed.Cont.Class
  ( class IxMonadCont
  , icallCC
  ) where

import Control.Category ((<<<))
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.Indexed (class IxMonad)
import Data.Indexed (Indexed(..))

-- | The `MonadCont` type class represents those monads which support the
-- | `callCC`, or _call-with-current-continuation_ operation.
-- |
-- | This action makes the current continuation available to the caller.
-- |
-- | For example:
-- |
-- | ```purescript
-- | delay :: forall eff. Number -> ContT Unit (Eff (timeout :: Timeout | eff)) Unit
-- | delay n = callCC \cont ->
-- |   lift $ setTimeout n (runContT (cont unit) (\_ -> return unit))
-- | ```
-- | An implementation is provided for `ContT`, and for other monad transformers
-- | defined in this library.
class IxMonad m <= IxMonadCont m where
  icallCC :: forall a x. ((forall b. a -> m x x b) -> m x x a) -> m x x a

instance ixMonadContIndexed :: MonadCont m => IxMonadCont (Indexed m) where
  icallCC f = Indexed do
    callCC \k -> case f (Indexed <<< k) of (Indexed m) -> m
