module UnfoldTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = go (f a) where
     go Nothing = Leaf
     go (Just (a,b,c)) = Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold f 0 where
     f z = case (z == x) of 
          True -> Nothing
          False -> Just (z+1,z,z+1)