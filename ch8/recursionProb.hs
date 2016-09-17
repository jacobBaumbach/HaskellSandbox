module RecursionProb where

sumNum :: (Eq a,Num a) => a -> a
sumNum a = go a 0 where
      go b acc
          | b==0 = acc
          | otherwise = go (b-1) (acc+b)

multNum :: Integral a => a -> a -> a

multNum a b = go a b 0 where
     go x y acc
          | x==0 = acc
          | otherwise = go (x-1) y (acc+y)
