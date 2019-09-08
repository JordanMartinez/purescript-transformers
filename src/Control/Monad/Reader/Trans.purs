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
-- import Control.Monad.Cont.Class (class MonadCont, callCC)
-- import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, class IxMonadReader, iask, iasks, ilocal)
-- import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Indexed.State.Class (class IxMonadState, istate)
import Control.Monad.Indexed.Trans.Class (class IxMonadTrans, ilift)
-- import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, pass, listen, tell)
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

instance functorIxReaderT :: IxFunctor m => IxFunctor (IxReaderT r m) where
  imap = mapIxReaderT <<< imap

instance applyIxReaderT :: IxApply m => IxApply (IxReaderT r m) where
  iapply (IxReaderT f) (IxReaderT v) = IxReaderT \r -> iapply (f r) (v r)

instance applicativeIxReaderT :: IxApplicative m => IxApplicative (IxReaderT r m) where
  ipure = IxReaderT <<< const <<< ipure

-- instance altIxReaderT :: Alt m => Alt (IxReaderT r m) where
--   alt (IxReaderT m) (IxReaderT n) = IxReaderT \r -> m r <|> n r
--
-- instance plusIxReaderT :: Plus m => Plus (IxReaderT r m) where
--   empty = IxReaderT (const empty)
--
-- instance alternativeIxReaderT :: Alternative m => Alternative (IxReaderT r m)

instance bindIxReaderT :: IxBind m => IxBind (IxReaderT r m) where
  ibind (IxReaderT m) k = IxReaderT \r ->
    ibind (m r) \a -> case k a of IxReaderT f -> f r

instance monadIxReaderT :: IxMonad m => IxMonad (IxReaderT r m)

-- instance monadZeroIxReaderT :: MonadZero m => MonadZero (IxReaderT r m)
--
-- instance semigroupIxReaderT :: (Apply m, Semigroup a) => Semigroup (IxReaderT s m a) where
--   append = lift2 (<>)
--
-- instance monoidIxReaderT :: (Applicative m, Monoid a) => Monoid (IxReaderT s m a) where
--   mempty = pure mempty
--
-- instance monadPlusIxReaderT :: MonadPlus m => MonadPlus (IxReaderT r m)

instance monadTransIxReaderT :: IxMonadTrans (IxReaderT r) where
  ilift = IxReaderT <<< const

-- instance monadEffectReader :: MonadEffect m => MonadEffect (IxReaderT r m) where
--   liftEffect = lift <<< liftEffect
--
-- instance monadContIxReaderT :: MonadCont m => MonadCont (IxReaderT r m) where
--   callCC f = IxReaderT \r -> callCC \c ->
--     case f (IxReaderT <<< const <<< c) of IxReaderT f' -> f' r
--
-- instance monadThrowIxReaderT :: MonadThrow e m => MonadThrow e (IxReaderT r m) where
--   throwError = lift <<< throwError
--
-- instance monadErrorIxReaderT :: MonadError e m => MonadError e (IxReaderT r m) where
--   catchError (IxReaderT m) h =
--     IxReaderT \r -> catchError (m r) (\e -> case h e of IxReaderT f -> f r)

instance monadAskIxReaderT :: IxMonad m => IxMonadAsk r (IxReaderT r m) where
  iask = IxReaderT ipure

instance monadReaderIxReaderT :: IxMonad m => IxMonadReader r (IxReaderT r m) where
  ilocal = withIxReaderT

instance monadStateIxReaderT :: IxMonadState s m => IxMonadState s (IxReaderT r m) where
  istate = ilift <<< istate

-- instance monadTellIxReaderT :: MonadTell w m => MonadTell w (IxReaderT r m) where
--   tell = lift <<< tell
--
-- instance monadWriterIxReaderT :: MonadWriter w m => MonadWriter w (IxReaderT r m) where
--   listen = mapIxReaderT listen
--   pass = mapIxReaderT pass
--
-- instance distributiveIxReaderT :: Distributive g => Distributive (IxReaderT e g) where
--   distribute a = IxReaderT \e -> collect (\r -> case r of IxReaderT r' -> r' e) a
--   collect f = distribute <<< map f
--
-- instance monadRecIxReaderT :: MonadRec m => MonadRec (IxReaderT r m) where
--   tailRecM k a = IxReaderT \r -> tailRecM (k' r) a
--     where
--     k' r a' = case k a' of IxReaderT f -> pure =<< f r
