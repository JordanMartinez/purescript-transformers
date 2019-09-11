-- | This module defines the writer monad transformer, `IxWriterT`.

module Control.Monad.Indexed.Writer.Trans
  ( IxWriterT(..), runIxWriterT, execIxWriterT, mapIxWriterT
  , module Control.Monad.Indexed.Trans.Class
  , module Control.Monad.Indexed.Writer.Class
  ) where

import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Alternative (class Alternative)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, ibind, imap, ipure)
import Control.Monad.Indexed.Cont.Class (class IxMonadCont, icallCC)
import Control.Monad.Indexed.Error.Class (class IxMonadThrow, class IxMonadError, icatchError, ithrowError)
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, class IxMonadReader, iask, ilocal)
-- import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Indexed.State.Class (class IxMonadState, istate)
import Control.Monad.Indexed.Trans.Class (class IxMonadTrans, ilift)
import Control.Monad.Indexed.Writer.Class (class IxMonadTell, itell, class IxMonadWriter, icensor, ilisten, ilistens, ipass)
-- import Control.MonadPlus (class MonadPlus)
-- import Control.MonadZero (class MonadZero)
-- import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), snd)
-- import Effect.Class (class MonadEffect, liftEffect)

-- | The writer monad transformer.
-- |
-- | This monad transformer extends the base monad with a monoidal accumulator of
-- | type `w`.
-- |
-- | The `MonadWriter` type class describes the operations supported by this monad.
newtype IxWriterT w m x y a = IxWriterT (m x y (Tuple a w))

-- | Run a computation in the `IxWriterT` monad.
runIxWriterT :: forall w m x y a. IxWriterT w m x y a -> m x y (Tuple a w)
runIxWriterT (IxWriterT x) = x

-- | Run a computation in the `IxWriterT` monad, discarding the result.
execIxWriterT :: forall w m x y a. IxFunctor m => IxWriterT w m x y a -> m x y w
execIxWriterT (IxWriterT m) = imap snd m

-- | Change the accumulator and base monad types in a `IxWriterT` monad action.
mapIxWriterT :: forall w1 w2 m1 m2 x y a b. (m1 x y (Tuple a w1) -> m2 x y (Tuple b w2)) -> IxWriterT w1 m1 x y a -> IxWriterT w2 m2 x y b
mapIxWriterT f (IxWriterT m) = IxWriterT (f m)

derive instance newtypeIxWriterT :: Newtype (IxWriterT w m x y a) _

instance ixFunctorIxWriterT :: IxFunctor m => IxFunctor (IxWriterT w m) where
  imap f = mapIxWriterT (imap \(Tuple a w) -> Tuple (f a) w)

instance ixApplyIxWriterT :: (Semigroup w, IxApply m) => IxApply (IxWriterT w m) where
  iapply (IxWriterT f) (IxWriterT v) = IxWriterT
    let k (Tuple a w) (Tuple b w') = Tuple (a b) (w <> w')
    in k `imap` f `iapply` v

instance ixApplicativeIxWriterT :: (Monoid w, IxApplicative m) => IxApplicative (IxWriterT w m) where
  ipure a = IxWriterT (ipure (Tuple a mempty))

-- instance altIxWriterT :: Alt m => Alt (IxWriterT w m) where
--   alt (IxWriterT m) (IxWriterT n) = IxWriterT (m <|> n)
--
-- instance plusIxWriterT :: Plus m => Plus (IxWriterT w m) where
--   empty = IxWriterT empty
--
-- instance alternativeIxWriterT :: (Monoid w, Alternative m) => Alternative (IxWriterT w m)

instance ixBindIxWriterT :: (Semigroup w, IxBind m) => IxBind (IxWriterT w m) where
  ibind (IxWriterT m) k = IxWriterT $
    ibind m \(Tuple a w) ->
      case k a of
        IxWriterT wt ->
          imap (\(Tuple b w') -> Tuple b (w <> w')) wt

instance ixMonadIxWriterT :: (Monoid w, IxMonad m) => IxMonad (IxWriterT w m)

-- instance ixMonadRecIxWriterT :: (Monoid w, MonadRec m) => MonadRec (IxWriterT w m) where
--   tailRecM f a = IxWriterT $ tailRecM f' (Tuple a mempty)
--     where
--     f' (Tuple a' w) =
--       case f a' of IxWriterT wt ->
--         wt >>= \(Tuple m w1) ->
--           pure case m of
--             Loop x -> Loop (Tuple x (w <> w1))
--             Done y -> Done (Tuple y (w <> w1))
--
-- instance ixMonadZeroIxWriterT :: (Monoid w, MonadZero m) => MonadZero (IxWriterT w m)
--
-- instance ixMonadPlusIxWriterT :: (Monoid w, MonadPlus m) => MonadPlus (IxWriterT w m)

instance ixMonadTransIxWriterT :: Monoid w => IxMonadTrans (IxWriterT w) where
  ilift m = IxWriterT Ix.do
    a <- m
    ipure (Tuple a mempty)

-- instance ixMonadEffectWriter :: (Monoid w, MonadEffect m) => MonadEffect (IxWriterT w m) where
--   liftEffect = lift <<< liftEffect
--
instance ixMonadContIxWriterT :: (Monoid w, IxMonadCont m) => IxMonadCont (IxWriterT w m) where
  icallCC f = IxWriterT $ icallCC \c ->
    case f (\a -> IxWriterT $ c (Tuple a mempty)) of IxWriterT b -> b
--
instance ixMonadThrowIxWriterT :: (Monoid w, IxMonadThrow e m) => IxMonadThrow e (IxWriterT w m) where
  ithrowError = ilift <<< ithrowError

instance ixMonadErrorIxWriterT :: (Monoid w, IxMonadError e m) => IxMonadError e (IxWriterT w m) where
  icatchError (IxWriterT m) h = IxWriterT $ icatchError m (\e -> case h e of IxWriterT a -> a)

instance ixMonadAskIxWriterT :: (Monoid w, IxMonadAsk r m) => IxMonadAsk r (IxWriterT w m) where
  iask = ilift iask

instance ixMonadReaderIxWriterT :: (Monoid w, IxMonadReader r m) => IxMonadReader r (IxWriterT w m) where
  ilocal f = mapIxWriterT (ilocal f)

instance ixMonadStateIxWriterT :: (Monoid w, IxMonadState s m) => IxMonadState s (IxWriterT w m) where
  istate f = ilift (istate f)

instance ixMonadTellIxWriterT :: (Monoid w, IxMonad m) => IxMonadTell w (IxWriterT w m) where
  itell = IxWriterT <<< ipure <<< Tuple unit

instance ixMonadWriterIxWriterT :: (Monoid w, IxMonad m) => IxMonadWriter w (IxWriterT w m) where
  ilisten (IxWriterT m) = IxWriterT Ix.do
    Tuple a w <- m
    ipure (Tuple (Tuple a w) w)
  ipass (IxWriterT m) = IxWriterT Ix.do
    Tuple (Tuple a f) w <- m
    ipure (Tuple a (f w))
