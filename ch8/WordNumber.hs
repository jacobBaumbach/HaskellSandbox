module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
  | n == 9 = "nine"
  | n == 8 = "eight"
  | n == 7 = "seven"
  | n == 6 = "six"
  | n == 5 = "five"
  | n == 4 = "four"
  | n == 3 = "three"
  | n == 2 = "two"
  | n == 1 = "one"
  | n == 0 = "zero"

digits :: Int -> [Int]
digits n = go n [] where
     go x acc
       | x<10 = [x]++acc
       | otherwise = go c ([d]++acc) where 
       	   (c,d) = divMod x 10

wordNumber :: Int -> [Char]
wordNumber n = go y x where
	x:y = map digitToWord (digits n)
	go lst acc
	     | length lst ==0 = acc
	     | otherwise = go (tail lst) (acc++"-"++(head lst))