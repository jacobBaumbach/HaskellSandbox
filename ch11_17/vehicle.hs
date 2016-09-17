module Vehicle1 where

data Price =
     Price Integer deriving (Eq, Show)

data Size =
     Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CataputsR'Us | United deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane x = (not . isCar) x

areCars :: [Vehicle] -> [Bool]
areCars x = map isCar x

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x