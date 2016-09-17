module WNT where
import Test.QuickCheck.Gen (oneof)
data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
	arbitrary = oneOf [Fulse, Frue]

instance Arbitrary Fool where
	arbitrary = frequency [(2, Fulse),(1, Frue)]