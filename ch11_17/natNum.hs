module NatNum where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0::Integer
natToInteger (Succ a) = (1:: Integer) + (natToInteger a)

integerToNat :: Integer -> Maybe Nat
integerToNat x = case y of
                      True -> Nothing
                      False -> Just (go x)
                 where
                       y = x<0
                       go 0 = Zero
                       go x = Succ (go (x-1))