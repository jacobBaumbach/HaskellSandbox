module Papu1 where

data Rocks =
     Rocks String deriving (Eq, Show)

data Yeah =
     Yeah Bool deriving (Eq, Show)

data Papu =
     Papu Rocks Yeah
     deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
--above lines won't compile because the three data types
--need to also derive Ord