module FixedDiv where

go :: Integral a => (a -> a -> a) -> (a -> a -> Bool) -> a -> a -> a -> (a, a)
go f comp x y acc
     | comp x y = (negate acc, x)
     | otherwise = go f comp (f x y) y (f acc 1)
absVal :: Integral a => a -> a 
absVal x
     | x<0 = -x
     |otherwise = x

comp :: Integral a => a -> a -> Bool
comp x y = (<) (absVal x) (absVal y)
add :: Integral a => a -> a -> a
add = (+)
sub :: Integral a => a -> a -> a
sub = (-)


divMyOwn :: Integral a => a -> a -> (a, a)
divMyOwn num denom
 | (denom == 0 || num == 0) = (0,0)
 | ((num>0 && denom>0) || (num<0 && denom<0))= go sub comp num denom 0
 | otherwise = go add comp num denom 0 