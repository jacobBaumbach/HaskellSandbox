module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

ident :: a -> a
ident x = x

rtpf :: (Show a, Read a) => a -> a
rtpf = read . show

rtpf1 :: (Show a, Read b) => a -> b
rtpf1 = read . show

main = do
     print (roundTrip 4)
     print (ident 4)
     print (rtpf 4)
     print (rtpf1 4 :: Int)