module ZippingExercises where

zipWithMine :: (a -> b -> c) -> [a] -> [b] ->[c]

zipWithMine f a b = go a b [] where
     go x y acc
          | ((length x == 0) || (length y==0)) = acc
          | otherwise = go (tail x) (tail y) (acc++[f (head x) (head y)])

zipMine = zipWithMine (\x -> \y -> (,) x y)
