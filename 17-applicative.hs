-- ------------------------------------------------------------------------- --
-- Chapter  17    — Applicative
-- ------------------------------------------------------------------------- --

import           Data.List (elemIndex)
import           Control.Applicative (liftA3)

import           Test.Hspec (hspec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)

import           Test.QuickCheck hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes (functor, applicative)

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise       — Lookups
--
-- In the following exercises you will need to use the following terms to
-- make the expressions typecheck:
--
--      pure
--      (<$>)
--      (<*>)
--
-- Make the following expressions typecheck.

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise 1     — Lookups

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
--    = (+3)     (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise 2     — Lookups

tupled :: Maybe (Integer, Integer)
tupled =
  let
    y :: Maybe Integer
    y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
  in
    (,) <$> y <*> z
--  (,)     y     z

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise 3     — Lookups

maxed :: Maybe Int
maxed =
  let
    x :: Maybe Int
    x = elemIndex 3 [1, 2, 3, 4, 5]

    y :: Maybe Int
    y = elemIndex 4 [1, 2, 3, 4, 5]

    max' :: Int -> Int -> Int
    max' = max
  in
    max' <$> x <*> y
--  max'     x     y

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise 4     — Lookups

summed :: Maybe Integer
summed =
  let
    xs = [1, 2, 3]
    ys = [4, 5, 6]

    x :: Maybe Integer
    x = lookup 3 $ zip xs ys

    y :: Maybe Integer
    y = lookup 2 $ zip xs ys
  in
    fmap sum $ (,) <$> x <*> y
--       sum $ (,)     x     y

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise       — Identity Instance
--
-- Write an Applicative instance for Identity.

newtype Identity a
    =   Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity

    Identity f <*> Identity a = Identity (f a)

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise       — Constant Instance
--
-- Write an Applicative instance for Constant.

newtype Constant a b
    = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty

    Constant x <*> Constant y = Constant (x <> y)

-- ------------------------------------------------------------------------- --
-- Chapter  17.05 — Applicative in use
-- Exercise       — Fixer Upper
--
-- Given the function and values provided, use (<$>) from Functor, (<*>) and
-- pure from the Applicative typeclass to fill in missing bits of the broken
-- code to make it work.

-- 1.   const <$>      Just     "Hello" <*>      "World"
--      const <$>      Just     "Hello" <*> pure "World"
--      const <$> pure Just <*> "Hello" <*>      "World"

-- 2.   (,,,)     Just 90 <*> Just 10     Just "Tierness"          [1, 2, 3]
--      (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- ------------------------------------------------------------------------- --
-- Chapter  17.08 — ZipList Monoid
-- Exercise       — List Applicative
--
-- Implement the list Applicative. Writing a minimally complete Applicative
-- instance calls for writing the definitions of both pure and <*>. We're
-- going to provide a hint as well. Use the checkers library to validate
-- your Applicative instance.

data    List a
    =   Nil
    |   Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

-- Use the above and try using flatMap and fmap without explicitly pattern
-- matching on cons cells. You'll still need to handle the Nil cases.
--
-- flatMap ::            (a -> List b) -> List a -> List b
-- (=<<)   :: Monad m => (a ->    m b) ->    m a ->    m b

instance Applicative List where
    pure a = Cons a Nil

    _   <*> Nil = Nil
    Nil <*> _   = Nil
--  (Cons f fs) <*> as = append (f <$> as) (fs <*> as)

--  fs <*> as = fs >>= (\f -> as >>= (\a -> return (f a)))
--  fs <*> as = (\f -> (\a -> pure (f a)) `flatMap` as ) `flatMap` fs
--  fs <*> as = (\f -> f <$> as) `flatMap` fs
    fs <*> as = flatMap (flip fmap as) fs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency
        [ (1, return Nil)
        , (3, Cons <$> arbitrary <*> arbitrary)
        ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

testListApplicative :: IO ()
testListApplicative =
    hspec $ do
        describe "List" $ do
            prop "Functor"     $ quickBatch $ functor     triggerList
            prop "Applicative" $ quickBatch $ applicative triggerList
          where
            triggerList :: List (String, String, Int)
            triggerList = undefined

-- ------------------------------------------------------------------------- --
-- Chapter  17.08 — ZipList Monoid
-- Exercise       — ZipList Applicative
--
-- Implement the ZipList Applicative. Use the checkers library to validate
-- your Applicative instance.

newtype ZipList a
    =   ZipList (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n (Cons x xs)
    | n > 0     = Cons x $ take' (pred n) xs
    | otherwise = Nil
take' _ Nil     = Nil

cycle' :: List a -> List a
cycle' xs = xs'
  where
    xs' = xs `append` xs'

fromZipList :: ZipList a -> List a
fromZipList (ZipList as) = as

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' = take' 100 $ fromZipList xs
        ys' = take' 100 $ fromZipList ys

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure a = ZipList $ cycle' (pure a)

    _   <*> (ZipList Nil) = ZipList Nil
    (ZipList Nil) <*> _   = ZipList Nil
    ZipList (Cons f fs) <*> ZipList (Cons a as) =
        ZipList $ Cons (f a) (fromZipList $ ZipList fs <*> ZipList as)

instance (Arbitrary a) => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary

testZipListApplicative :: IO ()
testZipListApplicative = hspec $ do
    describe "ZipList (Laws)" $ do
        let triggerZipList :: ZipList (String, String, Int)
            triggerZipList = undefined

        prop "Functor"     $ quickBatch $ functor     triggerZipList
        prop "Applicative" $ quickBatch $ applicative triggerZipList

    describe "ZipList (Unit Tests)" $ do
        let toZipList :: [a] -> ZipList a
            toZipList xs = ZipList $ foldr Cons Nil xs

            fs  = toZipList [(+10), (+20), (+30), (+40)]
            as  = toZipList [5..8]
            bs  = toZipList [15, 26, 37, 48]

            as' = toZipList (repeat 7)
            bs' = toZipList [17, 27, 37, 47]

        prop "Apply finite lists"   $ (fs <*> as ) `shouldBe` bs
        prop "Apply infinite lists" $ (fs <*> as') `shouldBe` bs'

-- ------------------------------------------------------------------------- --
-- Chapter  17.08 — ZipList Monoid
-- Exercise       — Variations on Either
--
-- Validation has the same representation as Either, but it can be
-- different. The Functor will behave the same, but the Applicative will be
-- different. See above for an idea of how Validation should behave. Use the
-- checkers library.

data    Validation e a
    =   Failure e
    |   Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure $ e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success

    Success f <*> Success a  = Success $ f a
    Failure e <*> Failure e' = Failure $ e <> e'
    _         <*> Failure e  = Failure $ e
    Failure e <*> _          = Failure $ e

data    Errors
    =   DividedByZero
    |   StackOverflow
    |   MooglesChewedWires
    deriving (Eq, Show)

instance Arbitrary Errors where
    arbitrary = elements
        [ DividedByZero
        , StackOverflow
        , MooglesChewedWires
        ]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = frequency
        [ (1, Failure <$> arbitrary)
        , (3, Success <$> arbitrary)
        ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

type ConcretValidation = Validation [Errors] Int

testValidationApplicative :: IO ()
testValidationApplicative = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerValidation :: Validation [Errors] (String, String, Int)
            triggerValidation = undefined

        prop "Functor"     $ quickBatch $ functor     triggerValidation
        prop "Applicative" $ quickBatch $ applicative triggerValidation

    describe "Validation (Unit Tests)" $ do
        let f   = Success (+1)
            a   = Success 1
            b   = Success 2

            e   = Failure [StackOverflow]
            e'  = Failure [MooglesChewedWires]
            e'' = Failure [MooglesChewedWires, StackOverflow]

        prop "f <*> a == b  " $ (f <*> a) `shouldBe` (b   :: ConcretValidation)
        prop "f <*> e == e  " $ (f <*> e) `shouldBe` (e   :: ConcretValidation)
        prop "e <*> f == e  " $ (e <*> f) `shouldBe` (e   :: ConcretValidation)
        prop "e'<*> e == e''" $ (e'<*> e) `shouldBe` (e'' :: ConcretValidation)

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise
--
-- Given a type that has an instance of Applicative, specialize the types of
-- the methods.
--
-- 1.   []
--
--      pure  :: a -> [a]
--      (<*>) :: [(a -> b)] -> [a] -> [b]
--
-- 2.   IO
--
--      pure  :: a -> IO a
--      (<*>) :: IO (a -> b) -> IO a -> IO b
--
-- 3.   (,) m
--
--      pure  :: (Monoid m) => a -> (m, a)
--      (<*>) :: (Monoid m) => (m, (a -> b)) -> (m, a) -> (m, b)
--
-- 4.   (->) e
--
--      pure  :: a -> (e -> a)
--      (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 1
--
-- Write instances for the following datatypes. Confused? Write out what the
-- type should be. Use the checkers library to validate the instances.
--

data    Pair a
    =   Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a

    (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

testPairApplicative :: IO ()
testPairApplicative = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerPair :: Pair (String, String, Int)
            triggerPair = undefined

        prop "Functor"     $ quickBatch $ functor     triggerPair
        prop "Applicative" $ quickBatch $ applicative triggerPair

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 2

data    Two a b
    =   Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure b = Two mempty b

    (Two a' f) <*> (Two a b) = Two (a' <> a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

testTwoApplicative :: IO ()
testTwoApplicative = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerTwo :: Two String (String, String, Int)
            triggerTwo = undefined

        prop "Functor"     $ quickBatch $ functor     triggerTwo
        prop "Applicative" $ quickBatch $ applicative triggerTwo

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 3

data    Three a b c
    =   Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c

    (Three a' b' f) <*> (Three a b c) = Three (a' <> a) (b' <> b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    =>  Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

testThreeApplicative :: IO ()
testThreeApplicative = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerThree :: Three String String (String, String, Int)
            triggerThree = undefined

        prop "Functor"     $ quickBatch $ functor     triggerThree
        prop "Applicative" $ quickBatch $ applicative triggerThree

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 4

data    Three' a b
    =   Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure c = Three' mempty c c

    (Three' a' f f') <*> (Three' a b b') = Three' (a' <> a) (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

testThreeApplicative' :: IO ()
testThreeApplicative' = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerThree' :: Three' String (String, String, Int)
            triggerThree' = undefined

        prop "Functor"     $ quickBatch $ functor     triggerThree'
        prop "Applicative" $ quickBatch $ applicative triggerThree'

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 5

data    Four a b c d
    =   Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d

    (Four a' b' c' f) <*> (Four a b c d) =
        Four (a' <> a) (b' <> b) (c' <> c) (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    =>  Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

testFourApplicative :: IO ()
testFourApplicative = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerFour :: Four String String String (String, String, Int)
            triggerFour = undefined

        prop "Functor"     $ quickBatch $ functor     triggerFour
        prop "Applicative" $ quickBatch $ applicative triggerFour

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise 6

data    Four' a b
    =   Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b

    (Four' a1' a2' a3' f) <*> (Four' a1 a2 a3 b) =
        Four' (a1' <> a1) (a2' <> a2) (a3' <> a3) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

testFourApplicative' :: IO ()
testFourApplicative' = hspec $ do
    describe "Validation (Laws)" $ do
        let triggerFour' :: Four' String (String, String, Int)
            triggerFour' = undefined

        prop "Functor"     $ quickBatch $ functor     triggerFour'
        prop "Applicative" $ quickBatch $ applicative triggerFour'

-- ------------------------------------------------------------------------- --
-- Chapter  17.09 — Chapter Exercises
-- Exercise       — Combinations
--
-- Remember the vowels and stops exercise in the folds chapter? Write the
-- function to generate the possible combinations of three input lists using
-- liftA3 from Control.Applicative.

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

testCombinations :: IO ()
testCombinations = hspec $ do
    describe "Combination (Unit Tests)" $ do
        prop "Stops Vowels Stops" $ combos stops vowels stops `shouldBe`
            [(s, v, s') | s <- stops, v <- vowels, s' <- stops]
