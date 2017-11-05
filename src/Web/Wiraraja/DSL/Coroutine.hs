{-# LANGUAGE Rank2Types #-}

module Web.Wiraraja.DSL.Coroutine where

import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)

import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Bifunctor (Bifunctor(..), bimap)

import Web.Wiraraja.Free.Trans (FreeT, runFreeT, freeT, resume, liftFreeT)

infixr 2 $$
infixr 8 <~
infixr 3 /\
infixr 3 \/

type Co = FreeT

type Process = Co Identity

loop :: (Functor f, Monad m) => Co f m (Maybe a) -> Co f m a
loop me = go
  where
    go = do
        v <- me
        maybe go return v

runProcess :: Monad m => Process m a -> m a
runProcess = runFreeT (return . runIdentity)

fuseWith :: (Functor f, Functor g, Functor h, Monad m)
         => (forall b c d. (b -> c -> d) -> f b -> g c -> h d)
         -> Co f m a
         -> Co g m a
         -> Co h m a
fuseWith zap fs0 gs0 = freeT $ go (fs0, gs0)
  where
    go (fs, gs) = do
        next <- liftM2 (zap (,)) <$> (resume fs) <*> (resume gs)
        case next of
            Left a  -> return (Left a)
            Right o -> return (Right (fmap (\t -> freeT (go t)) o))

fuseWithL :: (Functor f, Functor g, Functor h, Monad m)
          => (forall b c d. (b -> c -> d) -> f b -> g c -> h d)
          -> Co f m a
          -> Co g m a
          -> Co h m a
fuseWithL zap fs0 gs0 = freeT $ go (fs0, gs0)
  where
    go (fs, gs) = runExceptT $ do
        l <- ExceptT (resume fs)
        r <- ExceptT (resume gs)
        return (fmap (freeT . go) (zap (,) l r))

data Yield o a = Yield o a

instance Functor (Yield o) where
    fmap f (Yield o a) = Yield o (f a)

instance Bifunctor Yield where
    bimap f g (Yield o a) = Yield (f o) (g a)

type Producer o = Co (Yield o)

type Source o m = forall a. Producer o m a

yield :: Monad m => o -> Producer o m ()
yield o = liftFreeT (Yield o ())

producer :: Monad m => m (Either o r) -> Producer o m r
producer recv = loop $ do
    e <- lift recv
    case e of
        Left o -> do
            yield o
            return Nothing
        Right r -> return (Just r)

newtype Await i a = Await (i -> a)

instance Functor (Await i) where
    fmap f (Await k) = Await (f . k)

type Consumer i = Co (Await i)

await :: Monad m => Consumer i m i
await = liftFreeT (Await id)

consumer :: Monad m => (i -> m (Maybe r)) -> Consumer i m r
consumer send = loop $ do
    a <- await
    lift (send a)

newtype Transform i o a = Transform (i -> (o, a))

instance Bifunctor (Transform i) where
    bimap f g (Transform k) = Transform (bimap f g . k)

instance Functor (Transform i o) where
    fmap = bimap id

type Transformer i o = Co (Transform i o)

transform :: Monad m => (i -> o) -> Transformer i o m ()
transform k = liftFreeT $ Transform $ \i -> (k i, ())

data CoTransform i o a = CoTransform o (i -> a)

instance Bifunctor (CoTransform i) where
    bimap f g (CoTransform o k) = CoTransform (f o) (g . k)

instance Functor (CoTransform i o) where
    fmap = bimap id

type CoTransformer i o = Co (CoTransform i o)

cotransform :: Monad m => o -> CoTransformer i o m i
cotransform o = freeT (return (Right (CoTransform o pure)))

transformCoTransformL :: Monad m
                      => Transformer i1 i2 m a
                      -> CoTransformer i2 o m a
                      -> CoTransformer i1 o m a
transformCoTransformL = fuseWith $ \f (Transform t) (CoTransform o c) ->
    CoTransform o $ \i1 ->
      let
        (i2, a) = t i1
      in
        f a (c i2)

transformCoTransformR :: Monad m
                      => CoTransformer i o1 m a
                      -> Transformer o1 o2 m a
                      -> CoTransformer i o2 m a
transformCoTransformR = fuseWith $ \f (CoTransform o1 c) (Transform t) ->
  let
    (o2, a) = t o1
  in
    CoTransform o2 ((`f` a) . c)

fuseCoTransform :: Monad m => Transformer i o m a -> CoTransformer o i m a -> Process m a
fuseCoTransform = fuseWith $ \f (Transform k) (CoTransform i c) ->
    Identity $ let (o, a) = k i in f a (c o)

($$) :: Monad m => Producer o m a -> Consumer o m a -> Process m a
($$) = fuseWith $ \f (Yield e a) (Await c) -> Identity (f a (c e))

pullFrom :: Monad m => Consumer o m a -> Producer o m a -> Process m a
pullFrom = fuseWithL $ \f (Await c) (Yield e a) -> Identity (f (c e) a)

(<~) :: Monad m => Consumer o m a -> Producer o m a -> Process m a
(<~) = pullFrom

transformProducer :: Monad m => Producer i m a -> Transformer i o m a -> Producer o m a
transformProducer = fuseWith $ \f (Yield i a) (Transform t) -> case t i of (o, b) -> Yield o (f a b)

transformConsumer :: Monad m => Transformer i o m a -> Consumer o m a -> Consumer i m a
transformConsumer = fuseWith $ \f (Transform t) (Await g) -> Await $ \i -> case t i of (o, a) -> f a (g o)

joinProducers :: Monad m => Producer o1 m a -> Producer o2 m a -> Producer (o1, o2) m a
joinProducers = fuseWith $ \f (Yield o1 a) (Yield o2 b) -> Yield (o1, o2) (f a b)

(/\) :: Monad m => Producer o1 m a -> Producer o2 m a -> Producer (o1, o2) m a
(/\) = joinProducers

joinConsumers :: Monad m => Consumer i1 m a -> Consumer i2 m a -> Consumer (i1, i2) m a
joinConsumers = fuseWith $ \f (Await k1) (Await k2) -> Await $ \(i1, i2) -> f (k1 i1) (k2 i2)

(\/) :: Monad m => Consumer i1 m a -> Consumer i2 m a -> Consumer (i1, i2) m a
(\/) = joinConsumers
