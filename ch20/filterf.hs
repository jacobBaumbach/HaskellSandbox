module Filterf where
import Data.Monoid
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f t = foldMap (\x -> if (f x) then pure x else mempty) t

tst :: [Int]
tst = filterF even [1..10]