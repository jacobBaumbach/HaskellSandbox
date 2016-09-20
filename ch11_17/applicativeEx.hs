module ApplicativeEx where

import Control.Applicative

newtype Identity a = Identity a deriving Show

instance Functor Identity where
	fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
	pure = Identity
	(<*>) (Identity a) (Identity a') = Identity $ a a'

data Pair a = Pair a a deriving Show

instance Functor Pair where
	fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
	pure a = Pair a a
	(<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')


data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
	fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
	pure a = Two mempty a
	(<*>) (Two a b) (Two a' b') = Two (mappend a a') (b b')

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
	fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
	pure a = Three mempty mempty a
	(<*>) (Three a b c) (Three a' b' c') = Three (mappend a a') (mappend b b') (c c')

data Three' a b = Three' a b b
instance Functor (Three' a) where
	fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
	pure b = Three' mempty b b
	(<*>) (Three' a b c) (Three' a' b' c') = Three' (mappend a a') (b b') (c c')

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
	fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
	pure a = Four mempty mempty mempty a
	(<*>) (Four a b c d) (Four a' b' c' d') = Four (mappend a a') (mappend b b') (mappend c c') (d d')

data Four' a b = Four' a a a b
instance Functor (Four' a) where
	fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
	pure a = Four' mempty mempty mempty a
	(<*>) (Four' a a' a'' b) (Four' c c' c'' d) = Four' (mappend a c) (mappend a' c') (mappend a'' c'') (b d)


