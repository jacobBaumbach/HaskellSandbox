module Poss where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
     fmap _ LolNope = LolNope
     fmap f (Possibly a) = Possibly (f a)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
	fmap _ (First a) = First a
	fmap f (Second b) = Second (f b)

