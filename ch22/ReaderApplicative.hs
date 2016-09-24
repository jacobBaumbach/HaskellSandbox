{-# LANGUAGE InstanceSigs #-}
module ReaderApplicative where
import Control.Applicative
import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a}

liftA2' :: Applicative f => (a -> b-> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

asks:: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
	fmap f (Reader a) = Reader (f . a) 

instance Applicative (Reader r) where
	pure a = Reader f where
		f _ = a
	(<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
	(Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
	return = pure
	(>>=) (Reader ra) aRb = join $ Reader $ \r -> aRb $ ra r

