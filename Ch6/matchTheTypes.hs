module MatchTheTypes where

import Data.List

i :: Num a => a
-- i :: a is too generic for 1
i = 1

f :: Float
--f:: Num a => a too generic for 1.0
f = 1.0

--f' :: Float
f' :: Fractional a => a
f' = 1.0

--f'' :: Float
f'' :: RealFrac a => a
f'' = 1.0


--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

--freud' :: a -> a
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund1 :: Int -> Int
sigmund1 x = myX
-- sigmund :: a ->a won't work because the return type
--is fixed to int you could do sigmund :: a -> Int


myX' = 1 :: Int
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a won't work since the
--definition of the func returns an Int you could do
-- one of two things either have myX' = 1 || Num a => a -> Int
sigmund' x = myX'

-- jung:: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)


--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
-- signifier :: Ord a => [a] -> a will not work becaues
-- mySort only accepts a list of Char not a list of
-- a where a is a subclass of Ord
--in signifier replace mySort with sort and the 
-- above type signature would work


