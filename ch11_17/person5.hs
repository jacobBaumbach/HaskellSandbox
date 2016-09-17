module Person5 where
import Control.Monad
import Data.Char

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown deriving (Eq,Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person

mkPerson name age
     | name /= "" && age>0 = Right (Person name age)
     | name =="" = Left NameEmpty
     | age < 0 = Left AgeTooLow
     | otherwise = Left $ PersonInvalidUnknown 
gimmePerson :: IO()
gimmePerson = forever $ do
	putStrLn "age: "
	age <- getLine
	putStrLn "name: "
	name <- getLine
	f (mkPerson name (read age :: Integer))
	where 
	     f (Left _) = putStrLn "nope"
	     f (Right _) = putStrLn "made a person"
     
