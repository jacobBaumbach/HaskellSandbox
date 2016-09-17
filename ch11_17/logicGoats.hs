{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LogicGoats where

class TooMany a where
     tooMany :: a -> Bool

instance TooMany Int where
     tooMany n = n>42

instance TooMany String where
     tooMany str = str>"jjjjjj"

instance TooMany (Int, String) where
	 tooMany (a,b) = (||) (tooMany a) (tooMany b)

instance TooMany (Int, Int) where
     tooMany (a, b) = (||) (tooMany a) (tooMany b)

newtype Goats =
     Goats Int deriving (Eq, Show, TooMany)

