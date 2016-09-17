module RewriteFold where

myOr :: [Bool] -> Bool
myOr [] = True
myOr x = foldr (||) False x

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = True
myAny f x = foldl (\z -> \y -> z || (f y)) False x

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 f x = myOr (map f x)

myElem :: Eq a => a -> [a] -> Bool
myElem a x = myAny1 (\b -> a==b) x

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a x = foldr (\x -> \y -> y || (x==a)) False x

myReverse :: [a] -> [a]
myReverse x = foldl (\x -> \y -> y:x) [] x

myMap :: (a -> b) -> [a] -> [b]
myMap f x = foldr (\a -> \b -> (f a):b) [] x

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f x = foldr g [] x where
     g a acc
        | f a = a:acc
        | otherwise = acc

squish :: [[a]] -> [a]
squish x = foldr (++) [] x

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f x = foldr (\x -> \y -> (f x)++y) [] x

squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f x = squish (myMap f x)

squishAgain :: [[a]] -> [a]
squishAgain x = squishMap (\x -> x) x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr g x xs where
     g a acc
        | (f a acc) == GT = a
        | otherwise = acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl g x xs where
     g a acc
          | (f a acc) == LT = a
          | otherwise = acc
