module GuardDuty where

avgGrade :: (Fractional a, Ord a) => a -> Char

avgGrade x
  | otherwise = 'N'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  | y < 0.6 = 'F'

 --does not work everything will be mapped to 'N'

 --does not work, i.e. take a value .99
 -- .99 is greater than .7 so it will be mapped
 -- to 'C' when it is obviously an 'A'

 pal :: Eq a => [a] -> Bool
 pal xs
   | xs == reverse xs = True
   | otherwise = False

numbers :: (Ord a, Num a) => a -> a
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1