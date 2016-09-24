module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1..3]
y = [4..6]
z = [7..9]

xs:: Maybe Integer
xs = lookup 3 $ zip x y

ys:: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) zm zm where
	zm = z' n

summed :: Num c => (c,c) -> c
summed (a , b) = a+b

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = a
fromMaybe' _ (Just b) = b

bolt :: Integer -> Bool
bolt a= (&&) (a>3) (a<8)

sequA a = sequenceA [(>3), (<8), even] a

s' = summed <$> ((,) <$> xs <*> ys)
t' = (sequA . (fromMaybe' 1)) s'


main :: IO ()
main = do
	print $ sequenceA [Just 2, Just 3, Just 1]
	print $ sequenceA [x,y]
	print $ sequenceA [xs,ys]
	print $ summed <$> ((,)<$> xs <*>ys)
	print $ summed <$> ((,)<$> xs <*>zs)
	print $ bolt 7
	print $ fmap bolt z
	print $ foldl (&&) True $ sequA 7
	print $ bolt <$> ys
	print $ bolt <$> (z' 5)