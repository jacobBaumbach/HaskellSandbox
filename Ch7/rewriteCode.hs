module RewriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
     where (_, d) = divMod x 10

hunsD :: Integral a => a -> a
hunsD x = tensDigit y
     where (y, _) = divMod x 10

foldBool :: a -> a -> Bool ->a
foldBool x y c
  | c = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y c =
   case c of
   	True -> x
   	False -> y

g :: (a -> b) -> (a, c) -> (b, c)

g f a = (b, c) where
     b = (f . fst) a
     c = snd a


