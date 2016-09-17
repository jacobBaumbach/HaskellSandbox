module Validate where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiouAEIOU"

mkWord :: String -> Maybe Word'
mkWord x = if (z) then Just (Word' x) else Nothing where
     (a,b) = foldl f (0,0) x
     z = a>=b
     f (a,b) c = if (d) then (a,b+1) else (a+1,b) where
          d = elem c vowels