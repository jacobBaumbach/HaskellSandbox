module LangEx where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x : xs) = (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph x = go x "" where
     go x acc
          | x == [] = acc
          | otherwise = go y z where
          	     chk a = not (a == '.')
          	     a = dropWhile chk x
          	     y = if (length a == 0) then a else dropWhile (not . chk) a
          	     (b : bs) = takeWhile chk x
          	     z = if (b == ' ') then acc ++" "++(((toUpper . head) bs) : bs)++"." else acc ++((toUpper b) : bs)++"."
