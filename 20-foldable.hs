-- ------------------------------------------------------------------------- --
-- Chapter  20    — Foldable
-- ------------------------------------------------------------------------- --

import           Prelude hiding ( sum
                                , product
                                , elem
                                , minimum
                                , maximum
                                , null
                                , length
                                )

import           Data.Monoid

import           Test.Hspec (hspec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes (foldable)

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 1     — Library Functions
--
-- Implement the functions in terms of foldMap or foldr from Foldable, then
-- try them out with multiple types that have Foldable instances.

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0
--  = getSum . foldMap Sum

testSum :: IO ()
testSum = hspec $ do
    describe "sum (Unit Tests)" $ do
        prop "List Empty"     $ sum []       `shouldBe` 0
        prop "List Singleton" $ sum [42]     `shouldBe` 42
        prop "List"           $ sum [1..10]  `shouldBe` 55
        prop "Maybe"          $ sum (Just 1) `shouldBe` 1
        prop "Maybe"          $ sum Nothing  `shouldBe` 0

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 2     — Library Functions

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1
--      = getProduct . foldMap Product

testProduct :: IO ()
testProduct = hspec $ do
    describe "product (Unit Tests)" $ do
        prop "List Empty"     $ product []       `shouldBe` 1
        prop "List Singleton" $ product [42]     `shouldBe` 42
        prop "List"           $ product [1..5]   `shouldBe` 120
        prop "Maybe"          $ product (Just 2) `shouldBe` 2
        prop "Maybe"          $ product Nothing  `shouldBe` 1

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 3     — Library Functions

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr (\x z -> z || x == a) False
--   a = getAny . foldMap (Any . (==a))

testElem :: IO ()
testElem = hspec $ do
    describe "elem (Unit Tests)" $ do
        prop "List Empty"     $ elem 1 []       `shouldBe` False
        prop "List Singleton" $ elem 1 [42]     `shouldBe` False
        prop "List"           $ elem 1 [1..5]   `shouldBe` True
        prop "Maybe"          $ elem 1 (Just 1) `shouldBe` True
        prop "Maybe"          $ elem 1 Nothing  `shouldBe` False

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 4     — Library Functions

newtype Min a = Min { getMin :: Maybe a }

instance (Ord a) => Semigroup (Min a) where
    Min Nothing <> b = b
    a <> Min Nothing = a
    Min (Just a) <> Min (Just b) = Min . Just $ (min a b)

instance (Ord a) => Monoid (Min a) where
    mempty = Min Nothing

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr go Nothing
    where
        go a (Just b)   = Just (min a b)
        go a       _    = Just a

--      = getMin . foldMap (Min . Just)

testMinimum :: IO ()
testMinimum = hspec $ do
    describe "minimum (Unit Tests)" $ do
        prop "List Empty"     $ minimum []       `shouldBe` (Nothing :: Maybe Int)
        prop "List Singleton" $ minimum [42]     `shouldBe` (Just 42)
        prop "List"           $ minimum [3,1,2]  `shouldBe` (Just  1)
        prop "Maybe"          $ minimum (Just 1) `shouldBe` (Just  1)
        prop "Maybe"          $ minimum Nothing  `shouldBe` (Nothing :: Maybe Int)

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 5     — Library Functions

newtype Max a = Max { getMax :: Maybe a }

instance (Ord a) => Semigroup (Max a) where
    Max Nothing <> b = b
    a <> Max Nothing = a
    Max (Just a) <> Max (Just b) = Max . Just $ (max a b)

instance (Ord a) => Monoid (Max a) where
    mempty = Max Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr go Nothing
    where
        go a (Just b)   = Just (max a b)
        go a       _    = Just a

--      = getMax . foldMap (Max . Just)

testMaximum :: IO ()
testMaximum = hspec $ do
    describe "maximum (Unit Tests)" $ do
        prop "List Empty"     $ maximum []       `shouldBe` (Nothing :: Maybe Int)
        prop "List Singleton" $ maximum [42]     `shouldBe` (Just 42)
        prop "List"           $ maximum [2,1,3]  `shouldBe` (Just  3)
        prop "Maybe"          $ maximum (Just 1) `shouldBe` (Just  1)
        prop "Maybe"          $ maximum Nothing  `shouldBe` (Nothing :: Maybe Int)

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 6     — Library Functions

null :: (Foldable t) => t a -> Bool
null = foldr (const . const False) True

--   = not . getAny . foldMap (const $ Any True)

testNull :: IO ()
testNull = hspec $ do
    describe "null (Unit Tests)" $ do
        prop "List Empty"     $ null []       `shouldBe` True
        prop "List Singleton" $ null [42]     `shouldBe` False
        prop "List"           $ null [1..]    `shouldBe` False
        prop "Maybe"          $ null (Just 1) `shouldBe` False
        prop "Maybe"          $ null Nothing  `shouldBe` True

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 7     — Library Functions

length :: (Foldable t) => t a -> Int
length = foldr (const succ) 0

--     = getSum . foldMap (const $ Sum 1)

testLength :: IO ()
testLength = hspec $ do
    describe "length (Unit Tests)" $ do
        prop "List Empty"     $ length []       `shouldBe` 0
        prop "List Singleton" $ length [42]     `shouldBe` 1
        prop "List"           $ length [1,2,3]  `shouldBe` 3
        prop "Maybe"          $ length (Just 1) `shouldBe` 1
        prop "Maybe"          $ length Nothing  `shouldBe` 0

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 8     — Library Functions

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

--     = foldMap (\x -> [x])

testToList :: IO ()
testToList = hspec $ do
    describe "toList (Unit Tests)" $ do
        prop "List Empty"     $ toList []       `shouldBe` ([] :: [Int])
        prop "List Singleton" $ toList [42]     `shouldBe` [42]
        prop "List"           $ toList [1,2,3]  `shouldBe` [1,2,3]
        prop "Maybe"          $ toList (Just 1) `shouldBe` [1]
        prop "Maybe"          $ toList Nothing  `shouldBe` ([] :: [Int])

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 9     — Library Functions

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

--   = foldr (<>) mempty

testFold :: IO ()
testFold = hspec $ do
    describe "fold (Unit Tests)" $ do
        prop "List Empty"     $ fold (map Sum [])       `shouldBe` 0
        prop "List Singleton" $ fold (map Sum [1])      `shouldBe` 1
        prop "List"           $ fold (map Sum [1,2,3])  `shouldBe` 6

-- ------------------------------------------------------------------------- --
-- Chapter  20.05 — Some basic derived operations
-- Exercise 10    — Library Functions
--
-- Define foldMap in terms of foldr.

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

testFoldMap :: IO ()
testFoldMap = hspec $ do
    describe "foldMap (Unit Tests)" $ do
        prop "List Empty"     $ foldMap' id (map Sum [])        `shouldBe` 0
        prop "List Singleton" $ foldMap' id (map Sum [1])       `shouldBe` 1
        prop "List"           $ foldMap' id (map Sum [1,2,3])   `shouldBe` 6

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 1
--
-- Write Foldable instances for the following datatypes.

data    Constant a b
    =   Constant b
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f z (Constant b) = f b z
    foldMap f (Constant b) = f b

instance (Arbitrary b) => Arbitrary (Constant a b)
  where
    arbitrary = Constant <$> arbitrary

testConstant :: IO ()
testConstant = hspec $ do
    describe "Constant (Laws)" $ do
        let trigger :: Constant String (String, Int, Sum Int, Int, Int)
            trigger = undefined

        prop "Foldable" $ quickBatch $ foldable trigger

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 2

data    Two a b
    =   Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z
    foldMap f (Two _ b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where
    arbitrary = Two <$> arbitrary <*> arbitrary

testTwo :: IO ()
testTwo = hspec $ do
    describe "Two (Laws)" $ do
        let trigger :: Constant String (String, Int, Sum Int, Int, Int)
            trigger = undefined

        prop "Foldable" $ quickBatch $ foldable trigger

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 3

data    Three a b c
    =   Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three _ _ c) = f c z
    foldMap f (Three _ _ c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

testThree :: IO ()
testThree = hspec $ do
    describe "Three (Laws)" $ do
        let trigger :: Constant String (String, Int, Sum Int, Int, Int)
            trigger = undefined

        prop "Foldable" $ quickBatch $ foldable trigger

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 4

data    Three' a b
    =   Three' a b b

instance Foldable (Three' a) where
    foldr f z (Three' _ b b') = f b (f b' z)
    foldMap f (Three' _ b b') = f b <> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)
  where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

testThree' :: IO ()
testThree' = hspec $ do
    describe "Three' (Laws)" $ do
        let trigger :: Constant String (String, Int, Sum Int, Int, Int)
            trigger = undefined

        prop "Foldable" $ quickBatch $ foldable trigger

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 5

data    Four a b
    =   Four a b b b
    deriving (Eq, Show)

instance Foldable (Four a) where
    foldr f z (Four _ b1 b2 b3) = f b1 (f b2 (f b3 z))
    foldMap f (Four _ b1 b2 b3) = f b1 <> f b2 <> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b)
  where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

testFour :: IO ()
testFour = hspec $ do
    describe "Four (Laws)" $ do
        let trigger :: Constant String (String, Int, Sum Int, Int, Int)
            trigger = undefined

        prop "Foldable" $ quickBatch $ foldable trigger

-- ------------------------------------------------------------------------- --
-- Chapter  20.06 — Chapter Exercises
-- Exercise 6
--
-- Write a filter function for Foldable types using foldMap.

filterF
    :: (Applicative f, Foldable t, Monoid (f a))
    => (a -> Bool) -> t a -> f a
filterF f = foldr go mempty
    where
        go x z
            | f x       = pure x <> z
            | otherwise = z
