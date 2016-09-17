module StandardFunc where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True : y) = True
myOr (False : y) = myOr y

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:y) = if (f x) then True else myAny f y

myElem :: Eq a => a -> [a] -> Bool
myElem val x = myAny (\y -> y == val) x

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 f (x:y) = if (x==f) then True else myElem1 f y

myReverse :: [a] -> [a]
myReverse x = go x [] where
     go y acc
          | (length y) == 0 = acc
          | otherwise = go (tail y) ((head y):acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x: y) = x ++ squish y

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:y) = (f x) ++ squishMap f y

squishAgain :: [[a]] -> [a]
squishAgain x = squishMap (\x -> x) x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f lst = go (tail lst) (head lst) where
     go l mx
          | (length l) == 0 = mx
          | otherwise = go (tail l) newMx where
          	  newMx = if (f mx (head l) == LT) then (head l) else mx

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy f lst = go (tail lst) (head lst) where
     go l mn
          | (length l) == 0 = mn
          | otherwise = go (tail l) newMn where
          	  newMn = if (f mn (head l) == GT) then (head l) else mn

myMax :: (Ord a) => [a] -> a
myMax x = myMaximumBy compare x

myMin :: (Ord a) => [a] -> a
myMin x = myMinBy compare x

