module Mc91 where

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = go lst [] where
     go (x:y) acc = go y (acc++[(f x)])
     go _ acc = acc

mc91 :: Integral a => a -> a

mc91 x
   | x > 100 = x - 10
   | otherwise = 91

mapMc91 = myMap mc91