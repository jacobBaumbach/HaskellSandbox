module WarmingUpReader where
import Data.Char

cap :: [Char] -> [Char]
cap x = map toUpper x

rev :: [Char] -> [Char]
rev x = reverse x

composed :: [Char] -> [Char]
composed x = (cap . rev) x

fmapped :: [Char] -> [Char]
fmapped x = fmap cap rev x

tst :: [Char] -> Bool
tst x = (fmapped x) == (composed x)

tupled :: [Char] -> ([Char], [Char])
tupled x = ((,) <$> rev <*> composed) x

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
	a <- rev
	b <- cap
	return (a,b)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind x = ( cap >>= ((,) <$> rev )) x 