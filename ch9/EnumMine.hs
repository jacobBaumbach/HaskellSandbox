module EnumMine where

enumMine :: (Enum a, Eq a, Ord a) => a -> a-> [a]
enumMine start stop = go start stop [] where
     go x y acc
          | x > y = []
          | x ==y = x:acc
          | otherwise = go x (pred y) (y:acc)