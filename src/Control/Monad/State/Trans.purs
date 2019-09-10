-- | This module defines the state monad transformer, `IxStateT`.

module Control.Monad.Indexed.State.Trans
  ( IxStateT(..), runIxStateT, evalIxStateT, execIxStateT, mapIxStateT, withIxStateT
  , module Control.Monad.Indexed.Trans.Class
  , module Control.Monad.Indexed.State.Class
  ) where

import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Alternative (class Alternative)
-- import Control.Lazy (class Lazy)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iap, ibind, imap, ipure)
import Control.Monad.Indexed.Cont.Class (class IxMonadCont, icallCC)
import Control.Monad.Indexed.Error.Class (class IxMonadThrow, class IxMonadError, icatchError, ithrowError)
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, class IxMonadReader, iask, ilocal)
-- import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Indexed.State.Class (class IxMonadState, iget, igets, imodify, imodify_, iput, istate)
import Control.Monad.Indexed.Trans.Class (class IxMonadTrans, ilift)
import Control.Monad.Indexed.Writer.Class (class IxMonadTell, itell, class IxMonadWriter, ilisten, ipass)
-- import Control.MonadPlus (class MonadPlus)
-- import Control.MonadZero (class MonadZero)
-- import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
-- import Effect.Class (class MonadEffect, liftEffect)

-- | The state monad transformer.
-- |
-- | This monad transformer extends the base monad with the operations `get`
-- | and `put` which can be used to model a single piece of mutable state.
-- |
-- | The `MonadState` type class describes the operations supported by this monad.
newtype IxStateT s m x y a = IxStateT (s -> m x y (Tuple a s))

-- | Run a computation in the `IxStateT` monad.
runIxStateT :: forall s m x y a. IxStateT s m x y a -> s -> m x y (Tuple a s)
runIxStateT (IxStateT s) = s

-- | Run a computation in the `IxStateT` monad, discarding the final state.
evalIxStateT :: forall s m x y a. IxFunctor m => IxStateT s m x y a -> s -> m x y a
evalIxStateT (IxStateT m) s = imap fst (m s)

-- | Run a computation in the `IxStateT` monad discarding the result.
execIxStateT :: forall s m x y a. IxFunctor m => IxStateT s m x y a -> s -> m x y s
execIxStateT (IxStateT m) s = imap snd (m s)

-- | Change the result type in a `IxStateT` monad action.
mapIxStateT :: forall s m1 m2 x y a b. (m1 x y (Tuple a s) -> m2 x y (Tuple b s)) -> IxStateT s m1 x y a -> IxStateT s m2 x y b
mapIxStateT f (IxStateT m) = IxStateT (f <<< m)

-- | Modify the final state in a `IxStateT` monad action.
withIxStateT :: forall s m x y a. (s -> s) -> IxStateT s m x y a -> IxStateT s m x y a
withIxStateT f (IxStateT s) = IxStateT (s <<< f)

derive instance newtypeIxStateT :: Newtype (IxStateT s m x y a) _

instance ixFunctorIxStateT :: IxFunctor m => IxFunctor (IxStateT s m) where
  imap f (IxStateT a) = IxStateT (\s -> imap (\(Tuple b s') -> Tuple (f b) s') (a s))

instance ixApplyIxStateT :: IxMonad m => IxApply (IxStateT s m) where
  iapply = iap

instance ixApplicativeIxStateT :: IxMonad m => IxApplicative (IxStateT s m) where
  ipure a = IxStateT \s -> ipure $ Tuple a s

-- instance altIxStateT :: (Monad m, Alt m) => Alt (IxStateT s m) where
--   alt (IxStateT x) (IxStateT y) = IxStateT \s -> x s <|> y s
--
-- instance plusIxStateT :: (Monad m, Plus m) => Plus (IxStateT s m) where
--   empty = IxStateT \_ -> empty
--
-- instance alternativeIxStateT :: (Monad m, Alternative m) => Alternative (IxStateT s m)

instance ixBindIxStateT :: IxMonad m => IxBind (IxStateT s m) where
  ibind (IxStateT x) f = IxStateT \s ->
    ibind (x s) \(Tuple v s') -> case f v of IxStateT st -> st s'

instance ixMonadIxStateT :: IxMonad m => IxMonad (IxStateT s m)

-- instance ixMonadRecIxStateT :: MonadRec m => MonadRec (IxStateT s m) where
--   tailRecM f a = IxStateT \s -> tailRecM f' (Tuple a s)
--     where
--     f' (Tuple a' s) =
--       case f a' of IxStateT st ->
--         st s >>= \(Tuple m s1) ->
--           pure case m of
--             Loop x -> Loop (Tuple x s1)
--             Done y -> Done (Tuple y s1)
--
-- instance ixMonadZeroIxStateT :: MonadZero m => MonadZero (IxStateT s m)
--
-- instance ixMonadPlusIxStateT :: MonadPlus m => MonadPlus (IxStateT s m)

instance ixMonadTransIxStateT :: IxMonadTrans (IxStateT s) where
  ilift m = IxStateT \s -> Ix.do
    x <- m
    ipure (Tuple x s)

-- instance lazyIxStateT :: Lazy (IxStateT s m a) where
--   defer f = IxStateT \s -> case f unit of IxStateT f' -> f' s
--
-- instance ixMonadEffectState :: MonadEffect m => MonadEffect (IxStateT s m) where
--   liftEffect = lift <<< liftEffect
--
instance ixMonadContIxStateT :: IxMonadCont m => IxMonadCont (IxStateT s m) where
  icallCC f = IxStateT \s -> icallCC \c ->
    case f (\a -> IxStateT \s' -> c (Tuple a s')) of IxStateT f' -> f' s
--
instance ixMonadThrowIxStateT :: IxMonadThrow e m => IxMonadThrow e (IxStateT s m) where
  ithrowError = ilift <<< ithrowError

instance ixMonadErrorIxStateT :: IxMonadError e m => IxMonadError e (IxStateT s m) where
  icatchError (IxStateT m) h =
    IxStateT \s -> icatchError (m s) (\e -> case h e of IxStateT f -> f s)

instance ixMonadAskIxStateT :: IxMonadAsk r m => IxMonadAsk r (IxStateT s m) where
  iask = ilift iask

instance ixMonadReaderIxStateT :: IxMonadReader r m => IxMonadReader r (IxStateT s m) where
  ilocal = mapIxStateT <<< ilocal

instance ixMonadStateIxStateT :: IxMonad m => IxMonadState s (IxStateT s m) where
  istate f = IxStateT (ipure <<< f)

instance ixMonadTellIxStateT :: IxMonadTell w m => IxMonadTell w (IxStateT s m) where
  itell = ilift <<< itell

instance ixMonadWriterIxStateT :: IxMonadWriter w m => IxMonadWriter w (IxStateT s m) where
  ilisten m = IxStateT \s ->
    case m of
      IxStateT m' -> Ix.do
        Tuple (Tuple a s') w <- ilisten (m' s)
        ipure (Tuple (Tuple a w) s')
  ipass m = IxStateT \s -> ipass
    case m of
      IxStateT m' -> Ix.do
        Tuple (Tuple a f) s' <- m' s
        ipure (Tuple (Tuple a s') f)
