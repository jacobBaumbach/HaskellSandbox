module ListApplicative where

import Control.Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

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


