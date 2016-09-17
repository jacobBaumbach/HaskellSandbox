module Div where

divMine :: Integral a => a -> a -> (a, a)

divMine num denom = go num denom 0 where 
    go n d c
        | n < d = (c, n)
        | otherwise = go (n - d) d (c + 1)               