module LiftA3Combos where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos a b c = liftA3 (,,) a b c

svs :: (Eq a, Ord a) => [a] -> [a] -> [(a,a,a)]
svs x y = [(a,b,c) | a <-x, b<-y, c<-x]

combosExplicit :: [a] -> [b] -> [c] -> [(a,b,c)]
combosExplicit a b c = (,,) <$> a <*> b <*> c

tst = (combos stops vowels stops) ==  (svs stops vowels)