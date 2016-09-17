module InstanceFunc where

newtype Identity a = Identity a deriving Show
instance Functor Identity where
	fmap f (Identity a) = Identity (f a) 

data Pair a = Pair a a deriving Show
instance Functor Pair where
     fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving Show
instance Functor (Two a) where
     fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c deriving Show
instance Functor (Three a b) where
     fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
	fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
	fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
	fmap f (Four' a b c d) = Four' a b c (f d)