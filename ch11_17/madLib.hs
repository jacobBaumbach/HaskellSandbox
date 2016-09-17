module MadLib where
import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = Noun
type Noun = String
type Exclamation = String

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e a n adj = mconcat [e,a,n,adj]