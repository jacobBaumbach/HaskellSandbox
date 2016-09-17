module Gardner1 where

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardner = String

data Garden =
     Either Gardner Gardenia
         Either (Gardner Daisy
             Either (Gardner Rose
                 Either(Gardner Lilac)))