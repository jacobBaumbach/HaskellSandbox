module Setence1 where

type Subject = String
type Verb = String
type Object = String

data Sentence =
     Sentence Subject Verb Object
     deriving (Eq, Show)

s1 = Sentence "dogs" "drool" --partially applied construction
--of Sentence equiv to a partially applied function
s2 = Sentence "Julie" "loves" "dogs"