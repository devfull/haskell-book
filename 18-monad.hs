-- ------------------------------------------------------------------------- --
-- Chapter  18    — Monad
-- ------------------------------------------------------------------------- --

import           Prelude hiding (Left, Right)

import           Control.Monad (join)

import           Test.Hspec (hspec, describe, shouldBe)
import           Test.Hspec.QuickCheck (prop)

import           Test.QuickCheck hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes (monoid, functor, applicative, monad)

-- ------------------------------------------------------------------------- --
-- Chapter  18.02 — The novel part of Monad
-- Exercise
--
-- Write bind in terms of fmap and join.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

-- ------------------------------------------------------------------------- --
-- Chapter  18.04 — Examples of Monad use
-- Exercise       — Either Monad
--
-- Implement the Either Monad.

data    Sum a b
    =   First a
    |   Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First  a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second

    (First  a) <*> _          = First a
    _          <*> (First  a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure

    (First  a) >>= _  = First a
    (Second b) >>= f  = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = frequency
        [ (1, First  <$> arbitrary)
        , (3, Second <$> arbitrary)
        ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

testSumMonad :: IO ()
testSumMonad = hspec $ do
    describe "Sum (Laws)" $ do
        let trigger :: Sum String (String, String, Int)
            trigger = undefined

        prop "Functor"     $ quickBatch $ functor     trigger
        prop "Applicative" $ quickBatch $ applicative trigger
        prop "Monad"       $ quickBatch $ monad       trigger

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 1
-- Exercise 1
--
-- Write Monad instances for the following types. Use the QuickCheck
-- properties we showed you to validate your instances.

data    Nope a
    =   NopeDotJpg
    deriving (Eq, Show)

instance Functor (Nope) where
    fmap _ _ = NopeDotJpg

instance Applicative (Nope) where
    pure _  = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad (Nope) where
    return  = pure
    _ >>= _ = NopeDotJpg

instance (Arbitrary a) => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
    (=-=) = eq

testNopeMonad :: IO ()
testNopeMonad = hspec $ do
    describe "Nope (Laws)" $ do
        let trigger :: Nope (String, String, Int)
            trigger = undefined

        prop "Functor"     $ quickBatch $ functor     trigger
        prop "Applicative" $ quickBatch $ applicative trigger
        prop "Monad"       $ quickBatch $ monad       trigger

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 1
-- Exercise 2

data    PhhhbbtttEither b a
    =   Left  a
    |   Right b
    deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap f (Left  a) = Left (f a)
    fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither  b) where
    pure = Left

    (Left  f) <*> (Left  a) = Left (f a)
    (Right b) <*> _         = Right b
    _         <*> (Right b) = Right b

instance Monad (PhhhbbtttEither b) where
    return = pure

    (Left  a) >>= f = f a
    (Right b) >>= _ = Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
    arbitrary = frequency
        [ (3, Left  <$> arbitrary)
        , (1, Right <$> arbitrary)
        ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
    (=-=) = eq

testPhhhbbtttEitherMonad :: IO ()
testPhhhbbtttEitherMonad = hspec $ do
    describe "PhhhbbtttEither (Laws)" $ do
        let trigger :: PhhhbbtttEither String (String, String, Int)
            trigger = undefined

        prop "Functor"     $ quickBatch $ functor     trigger
        prop "Applicative" $ quickBatch $ applicative trigger
        prop "Monad"       $ quickBatch $ monad       trigger

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 1
-- Exercise 3
--
-- Write a Monad instance for Identity.

newtype Identity a
    =   Identity a
    deriving (Eq, Ord, Show)

instance Functor (Identity) where
    fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad (Identity) where
    return = pure
    (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

testIdentityMonad :: IO ()
testIdentityMonad = hspec $ do
    describe "Identity (Laws)" $ do
        let trigger :: Identity (String, String, Int)
            trigger = undefined

        prop "Functor"     $ quickBatch $ functor     trigger
        prop "Applicative" $ quickBatch $ applicative trigger
        prop "Monad"       $ quickBatch $ monad       trigger

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 1
-- Exercise 4

data    List a
    =   Nil
    |   Cons a (List a)
    deriving (Eq, Show)

instance Semigroup (List a) where
    (Cons x xs) <> ys   = Cons x (xs <> ys)
    _           <> ys   = ys

instance Monoid (List a) where
    mempty = Nil

instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative (List) where
    pure a = Cons a Nil

    Nil <*> _           = Nil
    _   <*> Nil         = Nil
    (Cons f fs) <*> as  = (f <$> as) <> (fs <*> as)

instance Monad (List) where
    return = pure

    Nil >>= _ = Nil
    (Cons a as) >>= f = (f a) <> (as >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency
        [ (1, return Nil)
        , (3, Cons <$> arbitrary <*> arbitrary)
        ]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

testListMonad :: IO ()
testListMonad = hspec $ do
    describe "List (Laws)" $ do
        let trigger :: List (String, String, Int)
            trigger = undefined

        prop "Monoid"      $ quickBatch $ monoid      trigger
        prop "Functor"     $ quickBatch $ functor     trigger
        prop "Applicative" $ quickBatch $ applicative trigger
        prop "Monad"       $ quickBatch $ monad       trigger

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 1
--
-- Write the following functions using the methods provided by Monad and
-- Functor. Using stuff like identity and composition is fine, but it has to
-- typecheck with types provided.

j :: Monad m => m (m a) -> m a
j = flip (>>=) id

testJoin :: IO ()
testJoin = hspec $ do
    describe "join (Unit Tests)" $ do
        prop "List"  $ j [[1, 2], [], [3]]  `shouldBe` [1, 2, 3]
        prop "Maybe" $ j (Just (Just 1))    `shouldBe` Just 1
        prop "Maybe" $ j (Just Nothing)     `shouldBe` (Nothing :: Maybe Int)
        prop "Maybe" $ j Nothing            `shouldBe` (Nothing :: Maybe Int)

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= return . f

-- f ma = f <$> ma

--      = liftM

testLiftM :: IO ()
testLiftM = hspec $ do
    describe "liftM (Unit Tests)" $ do
        prop "List"   $ l1 succ [1, 2, 3]  `shouldBe` [2, 3, 4]
        prop "Maybe"  $ l1 (const 2) (Just 1) `shouldBe` (Just 2 ::  Maybe Int)

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
    a <- ma
    b <- mb
    return (f a b)

-- f ma mb = ma >>= (\a -> mb >>= (\b -> return $ f a b))

--         = liftM2

testLiftM2 :: IO ()
testLiftM2 = hspec $ do
    describe "liftM2 (Unit Tests)" $ do
        prop "List"   $ l2 (+) [0, 1] [0, 2] `shouldBe` [0, 2, 1, 3]
        prop "Maybe"  $ l2 (+) (Just 1) Nothing `shouldBe` Nothing

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 4

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
    f <- mf
    a <- ma
    return (f a)

--ma mf = mf >>= (\f -> ma >>= (\a -> return $ f a))

--      = flip ap

testAp :: IO ()
testAp = hspec $ do
    describe "ap (Unit Tests)" $ do
        prop "List"   $ a [1, 2, 3] [id, succ] `shouldBe` [1, 2, 3, 2, 3, 4]
        prop "Maybe"  $ a [Nothing] [const (Just 1)] `shouldBe` [Just 1]

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 5

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (a:as) f = do
    b  <- f a
    bs <- meh as f
    return $ (b:bs)

--  []     _ = return []
--  (a:as) f = f a >>= \b -> (meh as f) >>= \bs -> return $ b:bs

--           = flip mapM

testMapM :: IO ()
testMapM = hspec $ do
    describe "mapM (Unit Tests)" $ do
        prop "List"   $ meh [0, 1]    (\x ->   [x, succ x]) `shouldBe` [[0, 1], [0, 2], [1, 1], [1, 2]]
        prop "Maybe"  $ meh [1, 2, 3] (\x -> Just (succ x)) `shouldBe` Just [2, 3, 4]

-- ------------------------------------------------------------------------- --
-- Chapter  18.07 — Chapter Exercises Part 2
-- Exercise 6

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id
--       = sequence

testSequence :: IO ()
testSequence = hspec $ do
    describe "sequence (Unit Tests)" $ do
        prop "List Maybe" $ flipType [Just 0, Just 1] `shouldBe` Just [0, 1]
