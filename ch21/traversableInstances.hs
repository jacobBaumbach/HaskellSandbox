module TraversableInstances where

newtype Identity a = Identity a deriving (Eq, Ord, Show)
--traverse :: Applicative f => (a -> f b) t a -> f t b

instance Functor Identity where
	fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
	foldr f acc (Identity a) = f a acc

instance Traversable Identity where
	traverse f (Identity a) = fmap Identity $ f a

newtype Constant a b = Constant {getConstant :: a}

instance Functor (Constant a) where
	fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
	foldr f acc (Constant a) = acc

instance Traversable (Constant a) where
	traverse f (Constant a) = pure $ Constant a 

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Functor Optional where
	fmap _ Nada = Nada
	fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
	foldr f acc Nada = acc
	foldr f acc (Yep a) = f a acc

instance Traversable Optional where
	traverse _ Nada = pure Nada
	traverse f (Yep a) = fmap Yep $ f a

data List a = Nil | Cons a (List a)

instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons a b) = Cons (f a) $ fmap f b

instance Foldable List where
	foldr f acc Nil = acc
	foldr f acc (Cons a b) = f a $ foldr f acc b

instance Traversable List where
	traverse f Nil = pure Nil
	traverse f (Cons a b) = Cons <$> (f a) <*> (traverse f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
	fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
	foldr f acc (Three a b c) = f c acc

instance Traversable (Three a b) where
	traverse f (Three a b c) = fmap (Three a b) $ f c

data Three' a b = Three' a b b

instance Functor (Three' a) where
	fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
	foldr f acc (Three' a b b') = f b' acc' where
		acc' = f b acc

instance Traversable (Three' a) where
	traverse f (Three' a b b') = (Three' a) <$> (f b) <*> (f b')

data S n a = S (n a) a

instance (Functor n) => Functor (S n) where
	fmap f (S a b) = S (fmap f a) (f b)

instance (Foldable n) => Foldable (S n) where
	foldr f acc (S n a) = f a (foldr f acc n) 

instance Traversable n => Traversable (S n) where
	traverse f (S a b) = S <$> (traverse f a) <*> (f b)
	