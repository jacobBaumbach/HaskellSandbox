module Mood1 where

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                    then Blah
                    else x
-- only acceptable inputs are Blah || Woot
-- won't run because (==) :: a -> a -> Bool and 9 and Woot
--		are not of the same type
-- I would have to add Ord to deriving for that to work
--		assuming we would want Blah < Woot