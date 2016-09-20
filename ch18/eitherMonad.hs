module EitherMonad where

import Control.Applicative

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
	fmap _ (First a) = First a
	fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
	pure = Second
	(<*>) (First a) _ = First a
	(<*>) _ (First a) = First a
	(<*>) (Second b) (Second b') = (Second $ b b')

instance Monad (Sum a) where
	return = pure
	(>>=) (First a) _ = First a
	(>>=) (Second b) f = f b