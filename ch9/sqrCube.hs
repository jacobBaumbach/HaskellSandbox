module SqrCube where

mySqr = [x^2 | x <- [1..5]]
myCUbe = [y^3 | y <- [1..5]]

listTuples :: [a] -> [b] -> [(a,b)]
listTuples a b = [(x,y) | x<-a, y<-b]
sqrCube = listTuples mySqr myCUbe
sqrCubeFilter = [(a,b) | a<-mySqr, b<-myCUbe, a<50, b<50]
numElem :: [a] -> Int
numElem a = length a