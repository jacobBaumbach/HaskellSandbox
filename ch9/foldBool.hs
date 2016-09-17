module FoldBool where

import Data.Bool

foldBool :: (a -> Bool) -> a -> [a] -> [a]
foldBool f alt lst = map (\x -> bool x alt (f x)) lst

foldBoolTst = foldBool (\x -> x==3) (-3) [1..10] 