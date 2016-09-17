module Cipher where
import Data.Char
ensureAlpha :: Int -> Int
ensureAlpha x = (mod (x-97) 26)+97


flt :: Char -> Bool
flt x = (y>=97) && (y<=122) where
     y = ord x

sameLen :: String -> String -> String
sameLen str1 str2 = go str1 str2 "" where
     go s t acc
          | s == "" = acc
          | (flt (head s)) && (t == "") = go (tail s) (tail str2) (acc ++ [(head str2)])
          | flt (head s) = go (tail s) (tail t) (acc ++ [(head t)])
          | otherwise = go (tail s) t (acc ++ [(head s)])

vcip :: String -> String -> String
vcip x y = zipWith f x z where
     z = sameLen x y
     a = head x
     b = head y
     bOrd = ord b
     f c d 
        | c == a = c
        | (not . flt) c = c
        | otherwise = (chr .ensureAlpha) (((ord d) - bOrd)+(ord c))
     