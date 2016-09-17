module BT where

data BinaryTree a =
     Leaf
   | Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
     | b==a = Node left a right
     | b<a = Node (insert' b left) a right
     | b>a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node x y z where
     x = mapTree f left
     y = f a
     z = mapTree f right

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapExpected = Node (Node Leaf 5 Leaf) 3 (Node Leaf 6 Leaf)

mapOkay =
     if mapTree (+1) testTree == mapExpected
     then print "ok"
     else error "test failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (b ++ c) where
     b = preorder left
     c = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = b ++ [a] ++ c where
     b = preorder left
     c = preorder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) = b ++ c ++ [a] where
     b = postOrder left
     c = postOrder right

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
     if preorder testTree' == [2,1,3]
     then putStrLn "yes"
     else putStrLn "no"

testInorder :: IO ()
testInorder = 
     if inorder testTree' == [1,2,3]
     then putStrLn "yes"
     else putStrLn "no"

testPostorder :: IO ()
testPostorder =
     if postOrder testTree' == [1,3,2]
     then putStrLn "yes"
     else putStrLn "no"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f c left where 
     c = f a (foldTree f b right)

main :: IO ()
main = do
     testPreorder
     testInorder
     testPostorder








