{-# LANGUAGE Rank2Types, ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Web.Wiraraja.Free.Trans
    ( FreeT
    , freeT
    , liftFreeT
    , hoistFreeT
    , bimapFreeT
    , interpret
    , resume
    , runFreeT
    ) where

import Control.Monad (ap, (<=<))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))

import Data.Bifunctor (bimap)

data FreeT f m a
    = FreeT (m (Either a (f (FreeT f m a))))
    | forall x. Bind (FreeT f m x) (x -> FreeT f m a)

-- | Unpack `FreeT`, exposing the first step of the computation.
resume :: (Functor f, Monad m) => FreeT f m a -> m (Either a (f (FreeT f m a)))
resume = go
  where
    go ft = case ft of
        FreeT fa          -> fa
        Bind (FreeT m1) k -> do
            et <- m1
            case et of
                Left a   -> go (k a)
                Right fc -> return $ Right (fmap (\h -> h >>= k) fc)
        Bind (Bind m k1) k2 -> go (m >>= (\z -> k1 z >>= k2))

instance (Functor f, Functor m) => Functor (FreeT f m) where
    fmap f (FreeT m)  = FreeT (fmap (bimap f (fmap (fmap f))) m)
    fmap f (Bind a k) = Bind a (fmap f . k)

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure a = FreeT (return (Left a))
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return   = pure
    ma >>= f = case ma of
        Bind a k -> Bind a (\x -> Bind (k x) f)
        fa       -> Bind fa f

instance Functor f => MonadTrans (FreeT f) where
    lift ma = FreeT (fmap Left ma)
    {-# INLINE lift #-}

instance (Functor f, MonadThrow m) => MonadThrow (FreeT f m) where
    throwM = lift . throwM

instance (Functor f, MonadIO m) => MonadIO (FreeT f m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Functor f, MonadReader r m) => MonadReader r (FreeT f m) where
    ask     = lift ask
    {-# INLINE ask #-}
    local f = hoistFreeT (local f)
    {-# INLINE local #-}

instance (Functor f, MonadBase b m) => MonadBase b (FreeT f m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}

instance (Functor f, MonadState s m) => MonadState s (FreeT f m) where
    get = lift get
    {-# INLINE get #-}
    put = lift . put
    {-# INLINE put #-}
    state f = lift (state f)

liftFreeT :: (Functor f, Monad m) => f a -> FreeT f m a
liftFreeT fa = FreeT (return (Right (fmap return fa)))

freeT :: m (Either a (f (FreeT f m a))) -> FreeT f m a
freeT = FreeT

hoistFreeT :: (Functor f, Functor n) => (forall x. m x -> n x) -> FreeT f m a -> FreeT f n a
hoistFreeT = bimapFreeT id

-- | Change the base functor `f` for a `FreeT` action.
interpret :: (Functor f, Functor m) => (forall x. f x -> g x) -> FreeT f m a -> FreeT g m a
interpret nf = bimapFreeT nf id

bimapFreeT :: (Functor f, Functor n)
           => (forall x. f x -> g x) -> (forall x. m x -> n x) -> FreeT f m a -> FreeT g n a
bimapFreeT nf nm (Bind a f) = Bind (bimapFreeT nf nm a) (bimapFreeT nf nm . f)
bimapFreeT nf nm (FreeT m) = FreeT $ fmap (fmap (nf . fmap (bimapFreeT nf nm))) (nm m)

runFreeT :: (Functor f, Monad m)
         => (f (FreeT f m a) -> m (FreeT f m a)) -> FreeT f m a -> m a
runFreeT interp = goresume
  where
    goresume = go <=< resume
    go (Left a)   = return a
    go (Right fc) = interp fc >>= goresume
