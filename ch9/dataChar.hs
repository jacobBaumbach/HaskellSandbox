module DataChar where

import Data.Char

acro :: String -> String

acro x = filter (\x' -> isUpper x') x

capitalize :: String -> String

capitalize x = nh : t where
	nh = (toUpper . head) x
	t = tail x

upperHead :: String -> Char
upperHead = (toUpper . head)

allCap :: String -> String
allCap x = go x "" where
            go y acc
             | (length y) == 0 = acc
             | otherwise = go (tail y) (acc ++ [(upperHead y)])