module Tkd2 where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = c == b where
            c = f a

arith :: Num b => (a -> b) -> Integer -> a ->b

arith f i a = b+ (fromInteger i) where
              b = f a