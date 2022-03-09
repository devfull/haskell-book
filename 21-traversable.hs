{-# LANGUAGE TypeApplications #-}

-- ------------------------------------------------------------------------- --
-- Chapter  21    — Traversable
-- ------------------------------------------------------------------------- --

import           Prelude             hiding ( Either, Left, Right )

import           Data.Monoid

import           Test.Hspec                 ( hspec, describe, shouldBe )
import           Test.Hspec.QuickCheck      ( prop )

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes    ( functor
                                            , applicative
                                            , foldable
                                            , traversable
                                            )

-- ------------------------------------------------------------------------- --

-- Concret types used in functor, applicative and traversable properties

concret :: m (String, Int, [Int])
concret = undefined

-- Concret types used in foldable properties

concretFoldable :: Foldable f => f (String, Int, [Int], Int, Int)
concretFoldable = undefined

-- ------------------------------------------------------------------------- --
-- Chapter  21.09 — Traversable instances
-- Exercise       — Either

data    Either a b
    =   Left a
    |   Right b
    deriving (Eq, Show)

instance Functor (Either a) where
    fmap _ (Left  a) = Left a
    fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
    pure = Right

    Left  a <*> _ = Left a
    Right f <*> b = fmap f b

instance Foldable (Either a) where
    foldMap _ (Left  _) = mempty
    foldMap f (Right b) = f b

    foldr _ z (Left  _) = z
    foldr f z (Right b) = f b z

instance Traversable (Either a) where
    traverse _ (Left  a) = Left  <$> pure a
    traverse f (Right b) = Right <$> f b

    sequenceA  (Left  a) = Left  <$> pure a
    sequenceA  (Right b) = Right <$> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = frequency
        [ (1, Left  <$> arbitrary)
        , (3, Right <$> arbitrary)
        ]

instance (Eq a, Eq b) => EqProp (Either a b) where
    (=-=) = eq

testEither :: IO ()
testEither = hspec $ do
    describe "Either (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Either String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Either String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Either String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Either String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 1     — Identity
--
-- Write a Traversable instance for Identity.

newtype Identity a
    =   Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity

    Identity f <*> a = f <$> a

instance Foldable Identity where
    foldMap f (Identity a) = f a
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a
    sequenceA  (Identity a) = Identity <$>   a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

testIdentity :: IO ()
testIdentity = hspec $ do
    describe "Identity (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Identity)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Identity)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Identity)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Identity)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 2     — Constant

newtype Constant a b
    =   Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty

    Constant a <*> Constant a' = Constant (a <> a')

instance Foldable (Constant a) where
    foldMap _ _ = mempty
    foldr _ z _ = z

instance Traversable (Constant a) where
    traverse _ (Constant a) = Constant <$> pure a
    sequenceA  (Constant a) = Constant <$> pure a

instance (Arbitrary a) => Arbitrary (Constant a b)
  where
    arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
    (=-=) = eq

testConstant :: IO ()
testConstant = hspec $ do
    describe "Constant (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Constant String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Constant String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Constant String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Constant String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 3     — Maybe

data    Optional a
    =   Nada
    |   Yep a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada     = Nada
    fmap f (Yep a)  = Yep (f a)

instance Applicative Optional where
    pure = Yep

    Nada  <*> _ = Nada
    Yep f <*> m = f <$> m

instance Foldable Optional where
    foldMap _ Nada      = mempty
    foldMap f (Yep a)   = f a

    foldr _ z Nada      = z
    foldr f z (Yep a)   = f a z

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

    sequenceA  Nada    = pure Nada
    sequenceA  (Yep a) = Yep <$>   a

instance (Arbitrary a) => Arbitrary (Optional a)
  where
    arbitrary = frequency
        [ (1, return Nada)
        , (3, Yep <$> arbitrary)
        ]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq

testOptional :: IO ()
testOptional = hspec $ do
    describe "Optional (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Optional)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Optional)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Optional)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Optional)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 4     — List

data    List a
    =   Nil
    |   Cons a (List a)
    deriving (Eq, Ord, Show)

instance Semigroup (List a) where
    Nil         <> as  = as
    as          <> Nil = as
    (Cons a as) <> as' = Cons a (as <> as')

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure a = Cons a Nil

    (Nil      ) <*> _  = Nil
    (Cons f fs) <*> as = (f <$> as) <> (fs <*> as)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

    foldr _ z Nil = z
    foldr f z (Cons a as) = f a (foldr f z as)

    foldl _ z Nil = z
    foldl f z (Cons a as) = foldl f (f z a) as

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

    sequenceA  Nil = pure Nil
    sequenceA  (Cons a as) = Cons <$>   a <*> sequenceA  as

instance (Arbitrary a) => Arbitrary (List a)
  where
    arbitrary = frequency
        [ (1, return Nil)
        , (3, Cons <$> arbitrary <*> arbitrary)
        ]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

testList :: IO ()
testList = hspec $ do
    describe "List (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(List)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(List)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(List)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(List)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 5     — Three

data    Three a b c
    =   Three a b c
    deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c)  = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c

    (Three a' b' f) <*> (Three a b c) = Three (a' <> a) (b' <> b) (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c
    foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c
    sequenceA  (Three a b c) = Three a b <$>   c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

testThree :: IO ()
testThree = hspec $ do
    describe "Three (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Three String String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Three String String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Three String String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Three String String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 6     — Pair

data    Pair a b
    =   Pair a b
    deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b)  = Pair a (f b)

instance (Monoid a) => Applicative (Pair a) where
    pure b = Pair mempty b

    (Pair a' f) <*> (Pair a b) = Pair (a' <> a) (f b)

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b
    foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b
    sequenceA  (Pair a b) = Pair a <$>   b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

testPair :: IO ()
testPair = hspec $ do
    describe "Pair (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Pair String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Pair String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Pair String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Pair String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 7     — Big

data    Big a b
    =   Big a b b
    deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b b')  = Big a (f b) (f b')

instance (Monoid a) => Applicative (Big a) where
    pure b = Big mempty b b

    (Big a' f1 f2) <*> (Big a b1 b2) = Big (a' <> a) (f1 b1) (f2 b2)

instance Foldable (Big a) where
    foldMap f (Big _ b1 b2) = f b1 <> f b2
    foldr f z (Big _ b1 b2) = f b1 (f b2 z)

instance Traversable (Big a) where
    traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2
    sequenceA  (Big a b1 b2) = Big a <$>   b1 <*>   b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

testBig :: IO ()
testBig = hspec $ do
    describe "Big (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Big String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Big String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Big String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Big String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 8     — Big

data    Bigger a b
    =   Bigger a b b b
    deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b1 b2 b3)  = Bigger a (f b1) (f b2) (f b3)

instance (Monoid a) => Applicative (Bigger a) where
    pure b = Bigger mempty b b b

    (Bigger a' f1 f2 f3) <*> (Bigger a b1 b2 b3) =
        Bigger (a' <> a) (f1 b1) (f2 b2) (f3 b3)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ b1 b2 b3) = f b1 <> f b2 <> f b3
    foldr f z (Bigger _ b1 b2 b3) = f b1 (f b2 (f b3 z))

instance Traversable (Bigger a) where
    traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3
    sequenceA  (Bigger a b1 b2 b3) = Bigger a <$>   b1 <*>   b2 <*>   b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

testBigger :: IO ()
testBigger = hspec $ do
    describe "Bigger (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Bigger String)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Bigger String)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Bigger String)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Bigger String)

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 9     — SkiFree

data    S  n a
    =   S (n a) a
    deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
    fmap f (S na a) = S (f <$> na) (f a)

instance (Applicative n) => Applicative (S n) where
    pure a = S (pure a) a

    (S nf f) <*> (S na a) = S (nf <*> na) (f a)

instance (Foldable n) => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a
    foldr f z (S na a) = foldr f (f a z) na

instance (Traversable n) => Traversable (S n) where
    traverse f (S na a) = S <$> traverse f na <*> f a
    sequenceA  (S na a) = S <$> sequenceA  na <*>   a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a)
  where
    (=-=) = eq

testS :: IO ()
testS = hspec $ do
    describe "S (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(S [])
        prop "Applicative" $ quickBatch $ applicative $ concret          @(S [])
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(S [])
        prop "Traversable" $ quickBatch $ traversable $ concret          @(S [])

-- ------------------------------------------------------------------------- --
-- Chapter  21.12 — Chapter Exercises
-- Exercise 10    — Tree

data    Tree a
    =   Empty
    |   Leaf a
    |   Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ (Empty      ) = Empty
    fmap f (Leaf a     ) = Leaf (f a)
    fmap f (Node t a t') = Node (f <$> t) (f a) (f <$> t')

instance Applicative Tree where
    pure a = Leaf a

    (_            ) <*>    (Empty      ) = Empty
    (Empty        ) <*>    (_          ) = Empty
    (Leaf    f    ) <*>    (Leaf   a   ) = Leaf (f a)
    (Leaf    f    ) <*>    (Node t a t') = Node (f  <$> t ) (f a) (f   <$> t')
    (Node tf f tf') <*> ta@(Leaf   a   ) = Node (tf <*> ta) (f a) (tf' <*> ta)
    (Node tf f tf') <*>    (Node t a t') = Node (tf <*> t ) (f a) (tf' <*> t')

instance Foldable Tree where
    foldMap _ (Empty      ) = mempty
    foldMap f (Leaf a     ) = f a
    foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'

    foldr _ z (Empty      ) = z
    foldr f z (Leaf a     ) = f a z
    foldr f z (Node t a t') = foldr f (f a (foldr f z t')) t

instance Traversable Tree where
    traverse f (Empty      ) = pure Empty
    traverse f (Leaf a     ) = Leaf <$> f a
    traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

    sequenceA  (Empty      ) = pure Empty
    sequenceA  (Leaf a     ) = Leaf <$>   a
    sequenceA  (Node t a t') = Node <$> sequenceA  t <*>   a <*> sequenceA  t'

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = frequency
        [ (1, return Empty)
        , (3, Leaf <$> arbitrary)
        , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
        ]

instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq

testTree :: IO ()
testTree = hspec $ do
    describe "Tree (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret          @(Tree)
        prop "Applicative" $ quickBatch $ applicative $ concret          @(Tree)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable  @(Tree)
        prop "Traversable" $ quickBatch $ traversable $ concret          @(Tree)
