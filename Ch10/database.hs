module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
                      DbNumber 9001,
                      DbNumber 9001,
                      DbString "Hello World!",
                      DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

filterDb :: (DatabaseItem -> Bool) -> (DatabaseItem -> a)-> [DatabaseItem] -> [a]
filterDb flt extract lst = map extract (filter flt lst)

filterDbDate x = filterDb flt extract x where
     extract (DbDate y) = y
     flt (DbDate y) = True
     flt _ = False

filterDbNumber x = filterDb flt extract x where
    extract (DbNumber y) = y
    flt (DbNumber y) = True
    flt _ = False

mostRecent :: [DatabaseItem] -> UTCTime

mostRecent x = foldr max acc tl where
	z = filterDbDate x
	acc = head z
	tl = tail z

sumDb :: [DatabaseItem] -> Integer
sumDb x = foldl (+) 0 z where
     z = filterDbNumber x

avgDb :: [DatabaseItem] -> Double
avgDb x = (fromInteger num)/denom where
	y = filterDbNumber x
	(num,denom) = foldr (\a -> \(b,c) -> (a+b,c+1.0)) (0,0.0) y





--filterDbDate :: [DatabaseItem] -> [UTCTime]
--filterDbDate x = map (\(DbDate y) -> y) lst where
--     lst = filter g x where
--                  g (DbDate y) = True
--                  g _ = False
