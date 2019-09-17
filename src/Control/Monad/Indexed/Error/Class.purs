-- | This module defines the `MonadError` type class and its instances.

module Control.Monad.Indexed.Error.Class where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Control.Monad.Indexed (class IxMonad, imap, ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..), either)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))

-- | The `MonadThrow` type class represents those monads which support errors via
-- | `throwError`, where `throwError e` halts, yielding the error `e`.
-- |
-- | An implementation is provided for `ExceptT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Left zero: `throwError e >>= f = throwError e`
-- |
class IxMonad m <= IxMonadThrow e m | m -> e where
  ithrowError :: forall x a. e -> m x x a

instance ixMonadThrowIndexed :: MonadThrow e m => IxMonadThrow e (Indexed m) where
  ithrowError = Indexed <<< throwError

-- | The `MonadError` type class represents those monads which support catching
-- | errors.
-- |
-- | - `catchError x f` calls the error handler `f` if an error is thrown during the
-- |   evaluation of `x`.
-- |
-- | An implementation is provided for `ExceptT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Catch: `catchError (throwError e) f = f e`
-- | - Pure: `catchError (pure a) f = pure a`
-- |
class IxMonadThrow e m <= IxMonadError e m | m -> e where
  icatchError :: forall x a. m x x a -> (e -> m x x a) -> m x x a

instance ixMonadErrorIndexed :: MonadError e m => IxMonadError e (Indexed m) where
  icatchError (Indexed m) h = Indexed do
    catchError m \e -> case h e of Indexed c -> c

-- | This function allows you to provide a predicate for selecting the
-- | exceptions that you're interested in, and handle only those exceptons.
-- | If the inner computation throws an exception, and the predicate returns
-- | Nothing, then the whole computation will still fail with that exception.
icatchJust
  :: forall e m x a b
   . IxMonadError e m
  => (e -> Maybe b) -- ^ Predicate to select exceptions
  -> m x x a        -- ^ Computation to run
  -> (b -> m x x a) -- ^ Handler
  -> m x x a
icatchJust p act handler = icatchError act handle
  where
  handle e =
    case p e of
      Nothing -> ithrowError e
      Just b -> handler b

-- | Return `Right` if the given action succeeds, `Left` if it throws.
itry
  :: forall e m x a
   . IxMonadError e m
  => m x x a
  -> m x x (Either e a)
itry a = (imap Right a) `icatchError` (ipure <<< Left)

-- | Make sure that a resource is cleaned up in the event of an exception. The
-- | release action is called regardless of whether the body action throws or
-- | returns.
iwithResource
  :: forall e m x r a
   . IxMonadError e m
  => m x x r
  -> (r -> m x x Unit)
  -> (r -> m x x a)
  -> m x x a
iwithResource acquire release kleisli = Ix.do
  resource <- acquire
  result <- itry (kleisli resource)
  release resource
  either ithrowError ipure result
