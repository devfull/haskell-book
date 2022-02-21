-- ------------------------------------------------------------------------- --
-- Chapter  15    — Monoid, Semigroup
-- ------------------------------------------------------------------------- --

import           Data.Monoid
import           Test.QuickCheck hiding (Success, Failure)

-- ------------------------------------------------------------------------- --
-- Chapter  15.10 — Reusing algebras by asking for algebras
-- Exercise       — Optional Monoid
--
-- Write the Monoid instance for our Maybe type renamed to Optional.

data  Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a)
  where
    Nada <> a = a
    a <> Nada = a
    (Only a) <> (Only b) = Only (a <> b)

instance (Monoid a) => Monoid (Optional a)
  where
    mempty = Nada

-- ------------------------------------------------------------------------- --
-- Chapter  15.12 — Better Living Through QuickCheck
-- Exercise       — Maybe Another Monoid
--
-- Write a Monoid instance for a Maybe type which doesn’t require a Monoid
-- for the contents. Reuse the Monoid law QuickCheck properties and use them
-- to validate the instance.

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a
    =   First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First' a)
  where
    (First' Nada) <> x = x
    x <> (First' Nada) = x
    a <> b = a

instance Monoid (First' a)
  where
    mempty = First' Nada

instance (Arbitrary a) => Arbitrary (First' a)
  where
    arbitrary = First' <$> frequency
        [ (1, return Nada)
        , (3, Only <$> arbitrary)
        ]

type    FirstMappend =
        First' String
    ->  First' String
    ->  First' String
    ->  Bool

type FstId = First' String -> Bool

testMonoidLaws = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
--
-- Given a datatype, implement the Semigroup instance. Validate all of your
-- instances with QuickCheck.

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 1

data  Trivial
    = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial
  where
    _ <> _ = Trivial

instance Arbitrary Trivial
  where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivialAssoc :: IO ()
testTrivialAssoc = quickCheck (semigroupAssoc :: TrivialAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 2

newtype Identity a
    =   Identity a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a)
  where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a)
  where
    arbitrary = Identity <$> arbitrary

type    IdentityAssoc a
    =   Identity a
    ->  Identity a
    ->  Identity a
    ->  Bool

testIdentityAssoc :: IO ()
testIdentityAssoc = quickCheck (semigroupAssoc :: IdentityAssoc Trivial)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 3

data  Two a b
    = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b)
  where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type    TwoAssoc a b
    =   Two a b
    ->  Two a b
    ->  Two a b
    ->  Bool

type    ConcretTwoAssoc
    =   TwoAssoc (Identity Trivial) Trivial

testTwoAssoc :: IO ()
testTwoAssoc = quickCheck (semigroupAssoc :: ConcretTwoAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 4

data  Three a b c
    = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c)
  where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type    ThreeAssoc a b c
    =   Three a b c
    ->  Three a b c
    ->  Three a b c
    ->  Bool

type    ConcretThreeAssoc
    =   ThreeAssoc (Identity Trivial) Trivial Trivial

testThreeAssoc :: IO ()
testThreeAssoc = quickCheck (semigroupAssoc :: ConcretThreeAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 5

data  Four a b c d
    = Four a b c d
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d)
  where
    (Four a b c d) <> (Four a' b' c' d') =
        Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d)
  where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type    FourAssoc a b c d
    =   Four a b c d
    ->  Four a b c d
    ->  Four a b c d
    ->  Bool

type    ConcretFourAssoc
    =   FourAssoc (Identity Trivial) Trivial Trivial Trivial

testFourAssoc :: IO ()
testFourAssoc = quickCheck (semigroupAssoc :: ConcretFourAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 6

newtype BoolConj
    =   BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj
  where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj
  where
    arbitrary = BoolConj <$> arbitrary

type    BoolConjAssoc
    =   BoolConj
    ->  BoolConj
    ->  BoolConj
    ->  Bool

testBoolConjAssoc :: IO ()
testBoolConjAssoc = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 7

newtype BoolDisj
    =   BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj
  where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj
  where
    arbitrary = BoolDisj <$> arbitrary

type    BoolDisjAssoc
    =   BoolDisj
    ->  BoolDisj
    ->  BoolDisj
    ->  Bool

testBoolDisjAssoc :: IO ()
testBoolDisjAssoc = quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 8

data  Or  a b
    = Fst a
    | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b)
  where
    (Snd b) <> _ = (Snd b)
    _       <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b)
  where
    arbitrary = frequency
        [ (1, Fst <$> arbitrary)
        , (1, Snd <$> arbitrary)
        ]

type    OrAssoc a b
    =   Or a b
    ->  Or a b
    ->  Or a b
    ->  Bool

testOrAssoc :: IO ()
testOrAssoc = quickCheck (semigroupAssoc :: OrAssoc Trivial Trivial)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 9

newtype Combine a b
    =   Combine { unCombine :: (a -> b) }

instance Show (Combine a b)
  where
    show (Combine _) = "Combine"

instance (Semigroup b) => Semigroup (Combine a b)
  where
    (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b)
  where
    arbitrary = Combine <$> arbitrary

type    CombineAssoc a b
    =   Combine a b
    ->  Combine a b
    ->  Combine a b
    ->  a
    ->  Bool

type    ConcretCombineAssoc
    =   CombineAssoc Int (Sum Int)

semigroupCombineAssoc :: (Eq b, Semigroup b) => CombineAssoc a b
semigroupCombineAssoc f g h x =
    unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

testCombineAssoc :: IO ()
testCombineAssoc = quickCheck (semigroupCombineAssoc :: ConcretCombineAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 10

newtype Comp a
    =   Comp { unComp :: (a -> a) }

instance Show (Comp a)
  where
    show (Comp _) = "Compose"

instance Semigroup (Comp a)
  where
    (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a)
  where
    arbitrary = Comp <$> arbitrary

type    CompAssoc a
    =   Comp a
    ->  Comp a
    ->  Comp a
    ->  a
    ->  Bool

semigroupCompAssoc :: (Eq a) => CompAssoc a
semigroupCompAssoc f g h x =
    unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

testCompAssoc :: IO ()
testCompAssoc = quickCheck (semigroupCompAssoc :: CompAssoc Int)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Semigroup Exercises
-- Exercise 11

data    Validation a b
    =   Failure a
    |   Success b
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b)
  where
    (Failure a) <> (Failure a') = Failure (a <> a')
    (Success b) <> _            = Success b
    _           <> (Success b)  = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b)
  where
    arbitrary = frequency
        [ (1, Failure <$> arbitrary)
        , (3, Success <$> arbitrary)
        ]

type    ValidationAssoc a b
    =   Validation a b
    ->  Validation a b
    ->  Validation a b
    ->  Bool

type    ConcretValidationAssoc
    =   ValidationAssoc Trivial Trivial

testValidationAssoc :: IO ()
testValidationAssoc = quickCheck (semigroupAssoc :: ConcretValidationAssoc)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 1
--
-- Given a datatype, implement the Monoid instance. Add Monoid constraints
-- to type variables where needed. For the datatypes you’ve already
-- implemented Semigroup instances for, you need to figure out what the
-- identity value is.

instance Monoid Trivial
  where
    mempty = Trivial

testTrivialIdentity :: IO ()
testTrivialIdentity = do
    quickCheck (monoidLeftIdentity  :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 2

instance (Monoid a) => Monoid (Identity a)
  where
    mempty = Identity mempty

testIdentityIdentity :: IO ()
testIdentityIdentity = do
    quickCheck (monoidLeftIdentity  :: Identity Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Identity Trivial -> Bool)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 3

instance (Monoid a, Monoid b) => Monoid (Two a b)
  where
    mempty = Two mempty mempty

type    ConcretTwo
    =   Two (Identity Trivial) Trivial

testTwoIdentity :: IO ()
testTwoIdentity = do
    quickCheck (monoidLeftIdentity  :: ConcretTwo -> Bool)
    quickCheck (monoidRightIdentity :: ConcretTwo -> Bool)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 4

instance Monoid BoolConj
  where
    mempty = BoolConj True

testBoolConjIdentity :: IO ()
testBoolConjIdentity = do
    quickCheck (monoidLeftIdentity  :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 5

instance Monoid BoolDisj
  where
    mempty = BoolDisj False

testBoolDisjIdentity :: IO ()
testBoolDisjIdentity = do
    quickCheck (monoidLeftIdentity  :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 6

instance (Monoid b) => Monoid (Combine a b)
  where
    mempty = Combine mempty

type    CombineIdentity a b
    =   Combine a b
    ->  a
    ->  Bool

type    ConcretCombineIdentity
    =   CombineIdentity Int (Sum Int)

monoidCombineLeftIdentity :: (Eq b, Monoid b) => CombineIdentity a b
monoidCombineLeftIdentity f x =
    unCombine (mempty <> f) x == unCombine f x

monoidCombineRightIdentity :: (Eq b, Monoid b) => CombineIdentity a b
monoidCombineRightIdentity f x =
    unCombine (f <> mempty) x == unCombine f x

testCombineIdentity :: IO ()
testCombineIdentity = do
    quickCheck (monoidCombineLeftIdentity  :: ConcretCombineIdentity)
    quickCheck (monoidCombineRightIdentity :: ConcretCombineIdentity)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 7

instance Monoid (Comp a)
  where
    mempty = Comp id

type    CompIdentity a
    =   Comp a
    ->  a
    ->  Bool

type    ConcretCompIdentity
    =   CompIdentity Int

monoidCompLeftIdentity :: (Eq a) => CompIdentity a
monoidCompLeftIdentity f x =
    unComp (mempty <> f) x == unComp f x

monoidCompRightIdentity :: (Eq a) => CompIdentity a
monoidCompRightIdentity f x =
    unComp (f <> mempty) x == unComp f x

testCompIdentity :: IO ()
testCompIdentity = do
    quickCheck (monoidCompLeftIdentity  :: ConcretCompIdentity)
    quickCheck (monoidCompRightIdentity :: ConcretCompIdentity)

-- ------------------------------------------------------------------------- --
-- Chapter  15.15 — Monoid Exercises
-- Exercise 8

newtype Mem s a
    =   Mem { runMem :: s -> (a, s) }

instance Show (Mem s a)
  where
    show (Mem _) = "Mem"

-- Semigroup

-- Wrong first attempt
--
-- instance (Semigroup a) => Semigroup (Mem s a)
--   where
--     (Mem f) <> (Mem g) =
--         Mem (f . snd . g)
--
-- Wrong second attempt
--
-- instance (Semigroup a) => Semigroup (Mem s a)
--   where
--     (Mem f) <> (Mem g) =
--         Mem ((\(a, s) -> (a, snd $ f s)) . g)

instance (Semigroup a) => Semigroup (Mem s a)
  where
    (Mem f) <> (Mem g) = Mem $ \s ->
        let (fa, fs) = f gs
            (ga, gs) = g s
        in
            (ga <> fa, fs)

instance (Arbitrary a, CoArbitrary s, Arbitrary s) => Arbitrary (Mem s a)
  where
    arbitrary = Mem <$> arbitrary

type    MemAssoc s a
    =   Mem s a
    ->  Mem s a
    ->  Mem s a
    ->  s
    ->  Bool

semigroupMemAssoc :: (Semigroup a, Eq a, Eq s) => MemAssoc s a
semigroupMemAssoc f g h x =
    (runMem (f <> (g <> h)) x) == (runMem ((f <> g) <> h) x)

testMemAssoc :: IO ()
testMemAssoc = quickCheck (semigroupMemAssoc :: MemAssoc Int String)

-- Monoid

instance (Monoid a) => Monoid (Mem s a)
  where
    mempty = Mem (\s -> (mempty, s))

type    MemIdentity s a
    =   Mem s a
    ->  s
    ->  Bool

type    ConcretMemIdentity
    =   MemIdentity Int String

monoidMemLeftIdentity :: (Monoid a, Eq a, Eq s) => MemIdentity s a
monoidMemLeftIdentity f x =
    runMem (mempty <> f) x == runMem f x

monoidMemRightIdentity :: (Monoid a, Eq a, Eq s) => MemIdentity s a
monoidMemRightIdentity f x =
    runMem (f <> mempty) x == runMem f x

testMemIdentity :: IO ()
testMemIdentity = do
    quickCheck (monoidMemLeftIdentity  :: ConcretMemIdentity)
    quickCheck (monoidMemRightIdentity :: ConcretMemIdentity)
