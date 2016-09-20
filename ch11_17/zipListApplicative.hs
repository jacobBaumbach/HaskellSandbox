module ZipListApplicative where
import Control.Applicative

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons a b) = Cons (f a) $ fmap f b 

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys


take' :: Int -> List a -> List a
take' a lst = go a lst Nil where
     go num Nil acc = acc
     go num (Cons c d) acc = if (num == 0) then acc else go (num-1) d $ append acc $ Cons c Nil

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
	(<*>) Nil _ = Nil
	(<*>) _ Nil = Nil
	(<*>) a b = flatMap (\x -> fmap x b) a

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
	fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
	pure a = ZipList' (pure a)
	(<*>) (ZipList' a) (ZipList' b) = ZipList' ((<*>) a b)


