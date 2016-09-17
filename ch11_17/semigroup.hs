module Semi where


import Data.Semigroup (Semigroup,
                       (<>))
import Test.QuickCheck (arbitrary,
                        Arbitrary,
                        CoArbitrary,
                        elements,
                        Gen,
                        quickCheck,
                        sample)

-- Semigroup exercises
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
--import Data.Semigroup (Semigroup, (<>))

--data Trivial = Trivial deriving (Eq, Show)

--instance  Semigroup Trivial where
--     _<>_ = Trivial

--instance Arbitrary Trivial where
--     arbitrary = return Trivial

--semigoupAssoc :: (Eq m, Semigroup m) => m -> m-> m-> Bool
--semigoupAssoc a b c = (a <>(b <> c)) == ((a<>b)<>c)

--type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--main :: IO ()
--main = do quickCheck (semigoupAssoc :: TrivialAssoc)