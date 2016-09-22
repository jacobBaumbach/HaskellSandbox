module TraversableTree where
import Control.Applicative

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
	fmap f Empty = Empty
	fmap f (Leaf a) = Leaf $ f a
	fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)

instance Foldable Tree where
	foldr f acc Empty = acc
	foldr f acc (Leaf a) = f a acc
	foldr f acc (Node a b c) = foldr f acc2 a where
		acc2 = f b acc1 where
			acc1 = foldr f acc c

instance Traversable Tree where
	traverse f Empty = pure Empty
	traverse f (Leaf a) = Leaf <$> (f a)
	traverse f (Node a b c) = liftA3 Node (traverse f a) (f b) (traverse f c)