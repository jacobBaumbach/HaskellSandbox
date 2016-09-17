module StrProcess where

notThe :: String -> Maybe String
notThe "" = Nothing
notThe x = a where
     y = notThe' x
     z = filter (\x -> x /="the") y
     a = if (z == []) then Nothing else Just $ (head z)++(foldl  (\x -> \y-> x++" " ++ y) "" (tail z)) 

replaceThe :: String -> String
replaceThe "" = ""
replaceThe x = tail (foldl f "" z) where
     z = notThe' x
     f acc x = acc ++ " " ++ y where
                 y = if (x=="the") then "a" else x


notThe' :: String -> [String]
notThe' x = go x [] where
     go y acc
          | y == "" = acc
          | not (elem ' ' y) = acc ++[y]
          | otherwise = go z newAcc where
               chk x = not (x == ' ')
               (_ : z) = dropWhile chk y
               newAcc = acc ++ [(takeWhile chk y)]

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = go (notThe' x) 0 0 where
     go y switch acc
          | y == [] = acc
          | (head y) == "the" = go (tail y) 1 acc
          | switch == 1 && (elem ((head . head) y) "aeiouAEIOU") = go (tail y) 0 (acc + 1)
          | otherwise = go (tail y) 0 acc



countVowels :: String -> Integer
countVowels x = foldl f 0 x where
     f acc y = acc + z where
          z = if (elem y "aeiouAeiou") then 1 else 0
