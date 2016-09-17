module Jammin where
import Data.List


data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Ord, Show)

data JamJars =
     Jam { frt :: Fruit,
           numbr :: Int} deriving (Eq, Ord, Show)

row1 = Jam Peach 10
row2 = Jam Plum 11
row3 = Jam Apple 12
row4 = Jam Blackberry 13
row5 = Jam Peach 14
row6 = Jam Plum 15
allJam = [row1,row2, row3, row4, row5, row6]

allTypesJam :: [JamJars] -> [Fruit]
allTypesJam x = map frt x

allNumJam :: [JamJars] -> [Int]
allNumJam x = map numbr x

allJamTypes = allTypesJam allJam
allJamNum = allNumJam allJam

totalJamJars :: [JamJars] -> Int
totalJamJars x = foldr (+) 0 z where
     z = allNumJam x

allJamTotal = totalJamJars allJam

compareKind :: Ord a => (JamJars -> a) -> JamJars -> JamJars -> Ordering
compareKind field a b = compare (field a) (field b)

compareNum = compareKind numbr
compareFruit = compareKind frt

sortJam :: (JamJars -> JamJars -> Ordering) -> [JamJars] -> [JamJars]
sortJam comp x = sortBy comp x

sortByNum x = sortJam compareNum x
sortByFruit x = sortJam compareFruit x

eqKind :: Eq a => (JamJars -> a) -> JamJars -> JamJars -> Bool
eqKind field a b = c == d where
     c = field a
     d = field b

eqNum = eqKind numbr
eqFruit = eqKind frt

grpByJam :: (JamJars -> JamJars -> Bool) -> [JamJars] -> [[JamJars]]
grpByJam eq a = groupBy eq a

grpByNum x = grpByJam eqNum (sortByNum x)
grpByFruit = grpByJam eqFruit (sortByFruit x)

