module MonadEx where
import Control.Applicative


data PhbbttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhbbttEither b) where
	fmap _ (Right' a) = Right' a
	fmap f (Left' a) = Left' $ f a

instance Applicative (PhbbttEither b) where
	pure = Left' 
	(<*>) (Right' a) _ = Right' a
	(<*>) _ (Right' a) = Right' a
	(<*>) (Left' a) (Left' b) = Left' $ a b

instance Monad (PhbbttEither b)  where
	return = pure
	(>>=) (Right' a) _ = Right' a
	(>>=) (Left' a) f = f a

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
	fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
	pure = Identity
	(<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
	return = pure
	(>>=) (Identity a) f = f a


data List a = Nil | Cons a (List a)

instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons a b) = Cons (f a) (fmap f b)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold f acc Nil = acc
fold f acc (Cons a b) = f a (fold f acc b)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f Nil = Nil
flatMap f lst = concat' $ fmap f lst

instance Applicative List where
	pure a = Cons a Nil
	(<*>) Nil x = Nil
	(<*>) x Nil = Nil
	(<*>) (Cons a b) (Cons c d) = flatMap (\a -> fmap a (Cons c d)) (Cons a b)

instance Monad List where
	return a = pure a
	(>>=) a f = flatMap f a


