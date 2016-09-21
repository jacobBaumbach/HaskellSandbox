module FoldLibraryFunc where
import Data.Monoid
sum' :: (Foldable t, Num a) => t a -> a
sum' t = getSum $ foldMap Sum t

product' :: (Foldable t, Num a) => t a -> a
product' t = getProduct $ foldMap Product t

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a t = foldr f False t where
	f x y = y || (x==a)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t = foldr f Nothing t where
	f x acc = if (y<acc) then y else acc where
		y = Just x

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' t = foldr f Nothing t where
     f x acc = if (y>acc) then y else acc where
     	y = Just x

null' :: (Foldable t) => t a -> Bool
null' t = foldr (\x -> \acc -> False) True t

length' :: (Foldable t) => t a -> Int
length' t = foldr f (0 :: Int) t where
     f x acc = 1 + acc

toList' :: (Foldable t) => t a -> [a]
toList' t = foldMap (\x -> [x]) t

fold' :: (Foldable t, Monoid m) => t m -> m
fold' t = foldr mappend mempty t

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = foldr g mempty t where
     g x y = mappend (f x) y