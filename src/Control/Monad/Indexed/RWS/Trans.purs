-- | This module defines the reader-writer-state monad transformer, `IxRWST`.

module Control.Monad.Indexed.RWS.Trans
  ( IxRWST(..), runIxRWST, evalIxRWST, execIxRWST, mapIxRWST, withIxRWST
  , module Control.Monad.Indexed.Trans.Class
  ) where

import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Alternative (class Alternative)
-- import Control.Lazy (class Lazy)
-- import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, ibind, imap, ipure)
import Control.Monad.Indexed.Error.Class (class IxMonadThrow, class IxMonadError, icatchError, ithrowError)
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, class IxMonadReader)
-- import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.RWS.Trans (RWSResult(..))
import Control.Monad.Indexed.State.Class (class IxMonadState)
import Control.Monad.Indexed.Trans.Class (class IxMonadTrans, ilift)
import Control.Monad.Indexed.Writer.Class (class IxMonadTell, class IxMonadWriter)
-- import Control.Plus (class Plus, empty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), uncurry)
-- import Effect.Class (class MonadEffect, liftEffect)

-- | The reader-writer-state monad transformer, which combines the operations
-- | of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.
newtype IxRWST r w s m x y a = IxRWST (r -> s -> m x y (RWSResult s a w))

-- | Run a computation in the `IxRWST` monad.
runIxRWST :: forall r w s m x y a. IxRWST r w s m x y a -> r -> s -> m x y (RWSResult s a w)
runIxRWST (IxRWST x) = x

-- | Run a computation in the `IxRWST` monad, discarding the final state.
evalIxRWST :: forall r w s m x y a. IxMonad m => IxRWST r w s m x y a -> r -> s -> m x y (Tuple a w)
evalIxRWST (IxRWST m) r s = ibind (m r s) \(RWSResult _ result writer) -> ipure (Tuple result writer)

-- | Run a computation in the `IxRWST` monad, discarding the result.
execIxRWST :: forall r w s m x y a. IxMonad m => IxRWST r w s m x y a -> r -> s -> m x y (Tuple s w)
execIxRWST (IxRWST m) r s = ibind (m r s) \(RWSResult state _ writer) -> ipure (Tuple state writer)

-- | Change the result and accumulator types in a `IxRWST` monad action.
mapIxRWST :: forall r w1 w2 s m1 m2 x y a1 a2. (m1 x y (RWSResult s a1 w1) -> m2 x y (RWSResult s a2 w2)) -> IxRWST r w1 s m1 x y a1 -> IxRWST r w2 s m2 x y a2
mapIxRWST f (IxRWST m) = IxRWST \r s -> f (m r s)

-- | Change the context type in a `IxRWST` monad action.
withIxRWST :: forall r1 r2 w s m x y a. (r2 -> s -> Tuple r1 s) -> IxRWST r1 w s m x y a -> IxRWST r2 w s m x y a
withIxRWST f m = IxRWST \r s -> uncurry (case m of IxRWST m' -> m') (f r s)

derive instance newtypeIxRWST :: Newtype (IxRWST r w s m x y a) _

instance functorIxRWST :: (IxFunctor m) => IxFunctor (IxRWST r w s m) where
  imap f (IxRWST m) = IxRWST \r s ->
    imap (\(RWSResult state result writer) -> RWSResult state (f result) writer) (m r s)

instance applyIxRWST :: (IxBind m, Monoid w) => IxApply (IxRWST r w s m) where
  iapply (IxRWST f) (IxRWST m) = IxRWST \r s ->
    ibind (f r s) \(RWSResult s' f' w') ->
    imap (\(RWSResult s'' a'' w'') -> RWSResult s'' (f' a'') (w' <> w'')) (m r s')

-- instance altIxRWST :: Alt m => Alt (IxRWST r w s m) where
--   alt (IxRWST m) (IxRWST n) = IxRWST $ \ r s -> m r s <|> n r s
--
-- instance alternativeIxRWST :: (Monoid w, Alternative m, Monad m) => Alternative (IxRWST r w s m)

instance bindIxRWST :: (IxBind m, Monoid w) => IxBind (IxRWST r w s m) where
  ibind (IxRWST m) f = IxRWST \r s ->
    ibind (m r s) \(RWSResult s' a w) ->
      case (f a) of
        IxRWST f' -> imap (\(RWSResult state result writer) ->
          RWSResult state result (w <> writer)) (f' r s')

instance applicativeIxRWST :: (IxMonad m, Monoid w) => IxApplicative (IxRWST r w s m) where
  ipure a = IxRWST \_ s -> ipure (RWSResult s a mempty)

instance monadIxRWST :: (IxMonad m, Monoid w) => IxMonad (IxRWST r w s m)

instance monadTransIxRWST :: Monoid w => IxMonadTrans (IxRWST r w s) where
  ilift m = IxRWST \_ s -> ibind m \a -> ipure (RWSResult s a mempty)

-- instance lazyIxRWST :: Lazy (IxRWST r w s m a) where
--   defer f = IxRWST \r s -> case f unit of IxRWST f' -> f' r s
--
-- instance monadEffectRWS :: (Monoid w, MonadEffect m) => MonadEffect (IxRWST r w s m) where
--   liftEffect = lift <<< liftEffect

instance monadAskIxRWST :: (IxMonad m, Monoid w) => IxMonadAsk r (IxRWST r w s m) where
  iask = IxRWST \r s -> ipure (RWSResult s r mempty)

instance monadReaderIxRWST :: (IxMonad m, Monoid w) => IxMonadReader r (IxRWST r w s m) where
  ilocal f m = IxRWST \r s -> case m of IxRWST m' -> m' (f r) s

instance monadStateIxRWST :: (IxMonad m, Monoid w) => IxMonadState s (IxRWST r w s m) where
  istate f = IxRWST \_ s -> case f s of Tuple a s' -> ipure (RWSResult s' a mempty)

instance monadTellIxRWST :: (IxMonad m, Monoid w) => IxMonadTell w (IxRWST r w s m) where
  itell w = IxRWST \_ s -> ipure (RWSResult s unit w)

instance monadWriterIxRWST :: (IxMonad m, Monoid w) => IxMonadWriter w (IxRWST r w s m) where
  ilisten m = IxRWST \r s ->
    case m of IxRWST m' ->
      ibind (m' r s) \(RWSResult s' a w) ->
        ipure (RWSResult s' (Tuple a w) w)
  ipass m = IxRWST \r s ->
    case m of IxRWST m' ->
      ibind (m' r s) \(RWSResult s' (Tuple a f) w) ->
        ipure (RWSResult s' a (f w))

instance monadThrowIxRWST :: (IxMonadThrow e m, Monoid w) => IxMonadThrow e (IxRWST r w s m) where
  ithrowError = ilift <<< ithrowError

instance monadErrorIxRWST :: (IxMonadError e m, Monoid w) => IxMonadError e (IxRWST r w s m) where
  icatchError m h = IxRWST $ \r s ->
    icatchError
      (case m of IxRWST m' -> m' r s)
      (\e -> case h e of IxRWST m' -> m' r s)

-- instance monadRecIxRWST :: (MonadRec m, Monoid w) => MonadRec (IxRWST r w s m) where
--   tailRecM k a = IxRWST \r s -> tailRecM (k' r) (RWSResult s a mempty)
--     where
--     k' r (RWSResult state result writer) =
--       case k result of
--         IxRWST m -> do
--           RWSResult state' result' writer' <- m r state
--           pure case result' of
--             Loop x -> Loop (RWSResult state' x (writer <> writer'))
--             Done y -> Done (RWSResult state' y (writer <> writer'))
--
-- instance plusIxRWST :: Plus m => Plus (IxRWST r w s m) where
--   empty = IxRWST \ _ _ -> empty
