module Sing where

fstString :: [Char] -> [Char]
fstString x = (++) x " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: Int -> [Char] 
sing z = if (z == 0) then fstString x else sndString y
     where x = "Singin"
           y = "Somewhere"