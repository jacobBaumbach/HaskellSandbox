module Addition where

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
     where go n d count
              | n<d = (count,n)
              | otherwise = go (n-d) d (count +1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy a b = go a b 0 where
     go c d acc
          | c==0 = acc
          | otherwise = go (c-1) d (acc+d)

main :: IO ()
main = hspec $ do
     describe "Addition" $ do
          it "1 + 1 is greater than 1" $ do
               (1 + 1) > 1 `shouldBe` True
          it "2 +2 is equal to 4" $ do
               (2 + 2) `shouldBe` 4
          it "15 divided by 3 is equal to 5" $ do
               dividedBy 15 3 `shouldBe` (5,0)
          it "17 divided by 4 is equal to 4 with remainder of 1" $ do
               dividedBy 17 4 `shouldBe` (4,1)
          it "5 times 4 is equal to 20" $ do
               multiplyBy 5 4 `shouldBe` 20
          it "11 times 11 is equal to 121" $ do
               multiplyBy 11 11 `shouldBe` 121
          it "x+1 is always greater than x" $ do
               property $ \x -> x+1 > (x:: Int)


genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
     a <- arbitrary
     b <- arbitrary
     return (a,b)


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
     a <- arbitrary
     b <-arbitrary
     elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
     a <- arbitrary
     elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
     a <- arbitrary
     frequency [(1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x+1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


prop_Bad :: Int -> Bool
prop_Bad x = (x+0) > x

runBad :: IO()
runBad = quickCheck prop_Bad