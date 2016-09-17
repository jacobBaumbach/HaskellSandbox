module Mon where

import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)

monoidAssoc :: (Eq m, Monoid m) => m -> m-> m-> Bool
monoidAssoc a b c = (a <>(b <> c)) == ((a<>b)<>c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mappend mempty a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mappend a mempty) == a



data Trivial= Trivial deriving (Eq, Show)

instance Monoid Trivial where
	mempty = Trivial
	mappend _ _ = Trivial

instance Arbitrary Trivial where
     arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialId = Trivial -> Bool

newtype Identity a = Identity a deriving Show
instance (Monoid a) => Monoid (Identity a) where
	mempty = Identity mempty
	mappend (Identity a) (Identity b) = Identity (mappend a b)

data Two a b = Two a b deriving Show
instance (Monoid a, Monoid b) => Monoid (Two a b) where
	mempty = Two mempty mempty
	mappend (Two c d) (Two e f) = Two (mappend c e) (mappend d f)

newtype BoolConj = BoolConj Bool
instance Monoid BoolConj where
	mempty = BoolConj True 
	mappend (BoolConj a) (BoolConj b) = BoolConj (a && b)

newtype BoolDisj = BoolDisj Bool
instance Monoid BoolDisj where
	mempty = BoolDisj False
	mappend (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

newtype Combine a b = Combine {unCombine :: (a -> b)}
instance (Monoid b) => Monoid (Combine a b) where
	mempty = Combine (\x -> mempty) 
	mappend (Combine f) (Combine g) = Combine (\x -> mappend (f x) (g x))

newtype Comp a = Comp (a -> a)
instance (Monoid a) => Monoid (Comp a) where
	mempty = Comp (\x -> mempty)
	mappend (Comp f) (Comp g) = Comp (\x -> (g . f) x)

newtype Mem s a = Mem { runMem :: s -> (a,s)}
instance  Monoid a => Monoid (Mem s a) where
	mempty = runMem (\x -> (mempty,s))
	mappend f g = runMem (\x -> ((mappend c e),f)) where
		(c, d) = f x
		(e, f) = g d