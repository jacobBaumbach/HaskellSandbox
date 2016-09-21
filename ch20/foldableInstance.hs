module FoldableInstance where

data Constant a b = Constant a

instance Foldable (Constant a) where
	foldr f i (Constant a) = i

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
	foldr f i (Two a b) = f b i

data Three a b c = Three a b c

instance Foldable (Three a b) where
 	foldr f i (Three a b c) = f c i

data Three' a b = Three' a b b

instance Foldable (Three' a) where
 	foldr f i (Three' a b b') = f b' i' where
 		i' = f b i

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
	foldr f i (Four' a b c d) = f d j where
		j = f c i' where
			i' = f b i