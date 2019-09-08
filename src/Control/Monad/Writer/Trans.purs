-- | This module defines the writer monad transformer, `WriterT`.

module Control.Monad.Indexed.Writer.Trans
  ( WriterT(..), runWriterT, execWriterT, mapWriterT
  , module Control.Monad.Indexed.Trans.Class
  , module Control.Monad.Indexed.Writer.Class
  ) where

import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Alternative (class Alternative)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, ibind, imap, ipure)
-- import Control.Monad.Cont.Class (class MonadCont, callCC)
-- import Control.Monad.Error.Class (class MonadThrow, class MonadError, catchError, throwError)
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
newtype WriterT w m x y a = WriterT (m x y (Tuple a w))

-- | Run a computation in the `WriterT` monad.
runWriterT :: forall w m x y a. WriterT w m x y a -> m x y (Tuple a w)
runWriterT (WriterT x) = x

-- | Run a computation in the `WriterT` monad, discarding the result.
execWriterT :: forall w m x y a. IxFunctor m => WriterT w m x y a -> m x y w
execWriterT (WriterT m) = imap snd m

-- | Change the accumulator and base monad types in a `WriterT` monad action.
mapWriterT :: forall w1 w2 m1 m2 x y a b. (m1 x y (Tuple a w1) -> m2 x y (Tuple b w2)) -> WriterT w1 m1 x y a -> WriterT w2 m2 x y b
mapWriterT f (WriterT m) = WriterT (f m)

derive instance newtypeWriterT :: Newtype (WriterT w m x y a) _

instance functorWriterT :: IxFunctor m => IxFunctor (WriterT w m) where
  imap f = mapWriterT (imap \(Tuple a w) -> Tuple (f a) w)

instance applyWriterT :: (Semigroup w, IxApply m) => IxApply (WriterT w m) where
  iapply (WriterT f) (WriterT v) = WriterT
    let k (Tuple a w) (Tuple b w') = Tuple (a b) (w <> w')
    in k `imap` f `iapply` v

instance applicativeWriterT :: (Monoid w, IxApplicative m) => IxApplicative (WriterT w m) where
  ipure a = WriterT (ipure (Tuple a mempty))

-- instance altWriterT :: Alt m => Alt (WriterT w m) where
--   alt (WriterT m) (WriterT n) = WriterT (m <|> n)
--
-- instance plusWriterT :: Plus m => Plus (WriterT w m) where
--   empty = WriterT empty
--
-- instance alternativeWriterT :: (Monoid w, Alternative m) => Alternative (WriterT w m)

instance bindWriterT :: (Semigroup w, IxBind m) => IxBind (WriterT w m) where
  ibind (WriterT m) k = WriterT $
    ibind m \(Tuple a w) ->
      case k a of
        WriterT wt ->
          imap (\(Tuple b w') -> Tuple b (w <> w')) wt

instance monadWriterT :: (Monoid w, IxMonad m) => IxMonad (WriterT w m)

-- instance monadRecWriterT :: (Monoid w, MonadRec m) => MonadRec (WriterT w m) where
--   tailRecM f a = WriterT $ tailRecM f' (Tuple a mempty)
--     where
--     f' (Tuple a' w) =
--       case f a' of WriterT wt ->
--         wt >>= \(Tuple m w1) ->
--           pure case m of
--             Loop x -> Loop (Tuple x (w <> w1))
--             Done y -> Done (Tuple y (w <> w1))
--
-- instance monadZeroWriterT :: (Monoid w, MonadZero m) => MonadZero (WriterT w m)
--
-- instance monadPlusWriterT :: (Monoid w, MonadPlus m) => MonadPlus (WriterT w m)

instance monadTransWriterT :: Monoid w => IxMonadTrans (WriterT w) where
  ilift m = WriterT Ix.do
    a <- m
    ipure (Tuple a mempty)

-- instance monadEffectWriter :: (Monoid w, MonadEffect m) => MonadEffect (WriterT w m) where
--   liftEffect = lift <<< liftEffect
--
-- instance monadContWriterT :: (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
--   callCC f = WriterT $ callCC \c ->
--     case f (\a -> WriterT $ c (Tuple a mempty)) of WriterT b -> b
--
-- instance monadThrowWriterT :: (Monoid w, MonadThrow e m) => MonadThrow e (WriterT w m) where
--   throwError e = lift (throwError e)
--
-- instance monadErrorWriterT :: (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
--   catchError (WriterT m) h = WriterT $ catchError m (\e -> case h e of WriterT a -> a)

instance monadAskWriterT :: (Monoid w, IxMonadAsk r m) => IxMonadAsk r (WriterT w m) where
  iask = ilift iask

instance monadReaderWriterT :: (Monoid w, IxMonadReader r m) => IxMonadReader r (WriterT w m) where
  ilocal f = mapWriterT (ilocal f)

instance monadStateWriterT :: (Monoid w, IxMonadState s m) => IxMonadState s (WriterT w m) where
  istate f = ilift (istate f)

instance monadTellWriterT :: (Monoid w, IxMonad m) => IxMonadTell w (WriterT w m) where
  itell = WriterT <<< ipure <<< Tuple unit

instance monadWriterWriterT :: (Monoid w, IxMonad m) => IxMonadWriter w (WriterT w m) where
  ilisten (WriterT m) = WriterT Ix.do
    Tuple a w <- m
    ipure (Tuple (Tuple a w) w)
  ipass (WriterT m) = WriterT Ix.do
    Tuple (Tuple a f) w <- m
    ipure (Tuple a (f w))
