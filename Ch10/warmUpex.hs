module WarmUpEx where

stop = "pbtdkg"
vowel = "aeiou"

noun = ["Obama", "Clinton", "Trump", "Bush","Biden"]
verb = ["helps","hurts","hinders","plays"]

svs :: (Eq a, Ord a) => [a] -> [a] -> [(a,a,a)]
svs x y = [(a,b,c) | a <-x, b<-y, c<-x]

svsFilter ::(Eq a, Ord a) =>  ((a,a,a) -> Bool) -> [a] -> [a] -> [(a,a,a)]
svsFilter f x y = filter f (svs x y)

startWithChar :: (Eq a, Ord a) => a -> [a] -> [a] -> [(a,a,a)]
startWithChar a b c = svsFilter (\(d,e,f) -> d==a) b c

startWithP x y = startWithChar 'p' x y