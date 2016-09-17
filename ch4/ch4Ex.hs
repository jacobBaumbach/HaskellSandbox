module Ch4Ex where

isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x>=0 then x else -x

f :: (a,b) -> (c,d) -> ((b,d),(a,c))

f a1 a2 = ((e,f),(g,h))
          where e = snd a1
                f = snd a2
                g = fst a1
                h = fst a2

x = (+)
g xs = w `x` 1
       where w = length xs

h = \x -> x
i = \(x : xs) -> x
j (a,b) = a

