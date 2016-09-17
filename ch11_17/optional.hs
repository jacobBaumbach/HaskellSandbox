module Opti where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
     mempty = Only (mempty)
     mappend Nada Nada = Only mempty
     mappend Nada (Only b) = Only b
     mappend (Only b) Nada = Only b
     mappend (Only b) (Only c) = Only (mappend b c)