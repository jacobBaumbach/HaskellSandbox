module Ch5Ex where

bigNum = (^) 5 $ 10
wahoo = (,) bigNum 10

x = print
y = print "woohoo!"
z = x "hello world"

a = (+)
b = a 5
c = b 10
d = b 200

aa c = 12+b
       where b = 10000*c

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x>y) then True else False

functionS :: (a,b) -> b
functionS (a,b) = b

i :: a -> a
i a = a

cc :: a -> b -> a
cc a b = a

ccc :: b -> a -> b
ccc b a = b

cc' :: a -> b -> b
cc' a b = b

r :: [a] -> [a]
r a = a

co :: (b -> c) -> (a -> b) -> (a -> c)
co second first = (second . first)

a' :: (a -> c) -> a -> a
a' f a = a

a'' :: (a -> b) -> a -> b
a'' f a = f a



