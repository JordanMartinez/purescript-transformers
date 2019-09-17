-- | This module defines the reader monad transformer, `IxReaderT`.

module Control.Monad.Indexed.Reader.Trans
  ( IxReaderT(..), runIxReaderT, withIxReaderT, mapIxReaderT
  , module Control.Monad.Indexed.Trans.Class
  , module Control.Monad.Indexed.Reader.Class
  ) where

import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Alternative (class Alternative)
-- import Control.Apply (lift2)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxFunctor, class IxApply, class IxApplicative, class IxBind, class IxMonad, imap, iapply, ipure, ibind)
import Control.Monad.Indexed.Cont.Class (class IxMonadCont, icallCC)
import Control.Monad.Indexed.Error.Class (class IxMonadThrow, class IxMonadError, icatchError, ithrowError)
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, class IxMonadReader, iask, iasks, ilocal)
-- import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Indexed.State.Class (class IxMonadState, istate)
import Control.Monad.Indexed.Trans.Class (class IxMonadTrans, ilift)
import Control.Monad.Indexed.Writer.Class (class IxMonadTell, class IxMonadWriter, ilisten, ipass, itell)
-- import Control.MonadPlus (class MonadPlus)
-- import Control.MonadZero (class MonadZero)
-- import Control.Plus (class Plus, empty)
-- import Data.Distributive (class Distributive, distribute, collect)
import Data.Newtype (class Newtype)
-- import Effect.Class (class MonadEffect, liftEffect)

-- | The reader monad transformer.
-- |
-- | This monad transformer extends the base monad transformer with a _global context_ of
-- | type `r`.
-- |
-- | The `MonadReader` type class describes the operations supported by this monad.
newtype IxReaderT r m x y a = IxReaderT (r -> m x y a)

-- | Run a computation in the `IxReaderT` monad.
runIxReaderT :: forall r m x y a. IxReaderT r m x y a -> (r -> m x y a)
runIxReaderT (IxReaderT x) = x

-- | Change the type of the result in a `IxReaderT` monad action.
mapIxReaderT :: forall r m1 m2 x y a b. (m1 x y a -> m2 x y b) -> IxReaderT r m1 x y a -> IxReaderT r m2 x y b
mapIxReaderT f (IxReaderT m) = IxReaderT (f <<< m)

-- | Change the type of the context in a `IxReaderT` monad action.
withIxReaderT :: forall r1 r2 m x y a. (r2 -> r1) -> IxReaderT r1 m x y a -> IxReaderT r2 m x y a
withIxReaderT f (IxReaderT m) = IxReaderT (m <<< f)

derive instance newtypeIxReaderT :: Newtype (IxReaderT r m x y a) _

instance ixFunctorIxReaderT :: IxFunctor m => IxFunctor (IxReaderT r m) where
  imap = mapIxReaderT <<< imap

instance ixApplyIxReaderT :: IxApply m => IxApply (IxReaderT r m) where
  iapply (IxReaderT f) (IxReaderT v) = IxReaderT \r -> iapply (f r) (v r)

instance ixApplicativeIxReaderT :: IxApplicative m => IxApplicative (IxReaderT r m) where
  ipure = IxReaderT <<< const <<< ipure

-- instance altIxReaderT :: Alt m => Alt (IxReaderT r m) where
--   alt (IxReaderT m) (IxReaderT n) = IxReaderT \r -> m r <|> n r
--
-- instance plusIxReaderT :: Plus m => Plus (IxReaderT r m) where
--   empty = IxReaderT (const empty)
--
-- instance alternativeIxReaderT :: Alternative m => Alternative (IxReaderT r m)

instance ixBindIxReaderT :: IxBind m => IxBind (IxReaderT r m) where
  ibind (IxReaderT m) k = IxReaderT \r ->
    ibind (m r) \a -> case k a of IxReaderT f -> f r

instance ixMonadIxReaderT :: IxMonad m => IxMonad (IxReaderT r m)

-- instance ixMonadZeroIxReaderT :: MonadZero m => MonadZero (IxReaderT r m)
--
-- instance semigroupIxReaderT :: (Apply m, Semigroup a) => Semigroup (IxReaderT s m a) where
--   append = lift2 (<>)
--
-- instance ixMonoidIxReaderT :: (Applicative m, Monoid a) => Monoid (IxReaderT s m a) where
--   mempty = pure mempty
--
-- instance ixMonadPlusIxReaderT :: MonadPlus m => MonadPlus (IxReaderT r m)

instance ixMonadTransIxReaderT :: IxMonadTrans (IxReaderT r) where
  ilift = IxReaderT <<< const

-- instance ixMonadEffectReader :: MonadEffect m => MonadEffect (IxReaderT r m) where
--   liftEffect = lift <<< liftEffect
--
instance ixMonadContIxReaderT :: IxMonadCont m => IxMonadCont (IxReaderT r m) where
  icallCC f = IxReaderT \r -> icallCC \c ->
    case f (IxReaderT <<< const <<< c) of IxReaderT f' -> f' r
--
instance ixMonadThrowIxReaderT :: IxMonadThrow e m => IxMonadThrow e (IxReaderT r m) where
  ithrowError = ilift <<< ithrowError

instance ixMonadErrorIxReaderT :: IxMonadError e m => IxMonadError e (IxReaderT r m) where
  icatchError (IxReaderT m) h =
    IxReaderT \r -> icatchError (m r) (\e -> case h e of IxReaderT f -> f r)

instance ixMonadAskIxReaderT :: IxMonad m => IxMonadAsk r (IxReaderT r m) where
  iask = IxReaderT ipure

instance ixMonadReaderIxReaderT :: IxMonad m => IxMonadReader r (IxReaderT r m) where
  ilocal = withIxReaderT

instance ixMonadStateIxReaderT :: IxMonadState s m => IxMonadState s (IxReaderT r m) where
  istate = ilift <<< istate

instance ixMonadTellIxReaderT :: IxMonadTell w m => IxMonadTell w (IxReaderT r m) where
  itell = ilift <<< itell

instance ixMonadWriterIxReaderT :: IxMonadWriter w m => IxMonadWriter w (IxReaderT r m) where
  ilisten = mapIxReaderT ilisten
  ipass = mapIxReaderT ipass
--
-- instance distributiveIxReaderT :: Distributive g => Distributive (IxReaderT e g) where
--   distribute a = IxReaderT \e -> collect (\r -> case r of IxReaderT r' -> r' e) a
--   collect f = distribute <<< map f
--
-- instance ixMonadRecIxReaderT :: MonadRec m => MonadRec (IxReaderT r m) where
--   tailRecM k a = IxReaderT \r -> tailRecM (k' r) a
--     where
--     k' r a' = case k a' of IxReaderT f -> pure =<< f r
