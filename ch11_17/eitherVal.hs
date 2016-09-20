module EitherVal where

import Control.Applicative

data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Sum a) where
	fmap _ (First a) = First a
	fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
	pure a = Second a
	(<*>) (First a) _ = First a
	(<*>) _ (First a) = First a
	(<*>) (Second a) (Second b) = Second $ a b

instance Functor (Validation e) where
	fmap _ (Error e) = Error e
	fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
	pure a = Success a
	(<*>) (Error e) (Error e') = Error $ mappend e e'
	(<*>) (Error e) _ = Error e
	(<*>) _ (Error e) = Error e
	(<*>) (Success a) (Success a') = Success $ a a'