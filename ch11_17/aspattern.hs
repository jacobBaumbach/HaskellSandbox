module AsPattern where
import Data.Char
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (xt@(x:xs)) (y:ys) = if (x == y) then isSubsequenceOf xs ys else isSubsequenceOf xt ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords x = go x [] where
     go y acc
          | y == [] = acc
          | otherwise = go tailList newAcc where
                 chk a = not (a==' ')
                 z = dropWhile chk y
                 tailList = if (length z == 0) then z else (tail z)
                 newAcc = acc ++[((b:bs),((toUpper b):bs))] where
          	     	(b:bs)=(takeWhile chk y) 