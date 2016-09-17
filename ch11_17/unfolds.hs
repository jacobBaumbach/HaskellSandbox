module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b = go (f b)
  where 
       go Nothing = []
       go (Just (a,c)) = a: myUnfoldr f c


betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (x , (f x))) a