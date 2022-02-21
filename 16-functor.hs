{-# LANGUAGE FlexibleInstances #-}

-- ------------------------------------------------------------------------- --
-- Chapter  16    — Functor
-- ------------------------------------------------------------------------- --

import           Test.QuickCheck
import           Test.QuickCheck.Function
import           GHC.Arr

-- ------------------------------------------------------------------------- --
-- Chapter  16.04 — Let's talk about f, baby
-- Exercise       — Be Kind
--
-- Given a type signature, determine the kinds of each type variable:
--
-- 1. What's the kind of a?
--    a -> a
--
--    a :: *
--
-- 2. What are the kinds of b and T? (The T is capitalized on purpose!)
--    a -> b a -> T (b a)
--
--    b :: * -> *
--    T :: * -> *
--
-- 3. What's the kind of c?
--    c a b -> c b a
--
--    c :: * -> * -> *
--
-- ------------------------------------------------------------------------- --
-- Chapter  16.07 — Commonly used functors
-- Exercise       — Heavy Lifting
--
-- Add fmap, parentheses, and function composition to the expression as
-- needed for the expression to typecheck and produce the expected result.
-- It may not always need to go in the same place, so don't get complacent.
--
--       (+1) $ read "[1]" :: [Int] == [2]
a = fmap (+1) $ read "[1]" :: [Int]

--                (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--       (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)

--  ((return '1' ++) . show)   (\x -> [x, 1..3]) $ 0 == "1[0,1,2,3]"
d = ((return '1' ++) . show) . (\x -> [x, 1..3])


e :: IO Integer -- e == 3693
e = let ioi = readIO "1" :: IO Integer
--      changed =      read       ("123"++)       show ioi
        changed = fmap read (fmap ("123"++) (fmap show ioi))
    in
--           (*3) changed
        fmap (*3) changed

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
    fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
    fmap (g . f) x == (fmap g . fmap f) x

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 1

newtype Identity a
    =   Identity a
    deriving (Eq, Show)

instance Functor (Identity) where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a)
  where
    arbitrary = Identity <$> arbitrary

type    IdentityIdentity a
    =   Identity a
    ->  Bool

type    IdentityCompose a
    =   Identity a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretIdentityIdentity = IdentityIdentity Int
type    ConcretIdentityCompose  = IdentityCompose Int

testIdentityFunctor = do
    quickCheck (functorIdentity :: ConcretIdentityIdentity)
    quickCheck (functorCompose  :: ConcretIdentityCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 2

data    Pair a
    =   Pair a a
    deriving (Eq, Show)

instance Functor (Pair) where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a)
  where
    arbitrary = Pair <$> arbitrary <*> arbitrary

type    PairIdentity a
    =   Pair a
    ->  Bool

type    PairCompose a
    =   Pair a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretPairIdentity = PairIdentity Int
type    ConcretPairCompose  = PairCompose Int

testPairFunctor = do
    quickCheck (functorIdentity :: ConcretPairIdentity)
    quickCheck (functorCompose  :: ConcretPairCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 3

data    Two a b
    =   Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where
    arbitrary = Two <$> arbitrary <*> arbitrary

type    TwoIdentity a b
    =   Two a b
    ->  Bool

type    TwoCompose a b
    =   Two a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretTwoIdentity = TwoIdentity Int Int
type    ConcretTwoCompose  = TwoCompose Int Int

testTwoFunctor = do
    quickCheck (functorIdentity :: ConcretTwoIdentity)
    quickCheck (functorCompose  :: ConcretTwoCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 4

data    Three a b c
    =   Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type    ThreeIdentity a b c
    =   Three a b c
    ->  Bool

type    ThreeCompose a b c
    =   Three a b c
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretThreeIdentity = ThreeIdentity Int Int Int
type    ConcretThreeCompose  = ThreeCompose Int Int Int

testThreeFunctor = do
    quickCheck (functorIdentity :: ConcretThreeIdentity)
    quickCheck (functorCompose  :: ConcretThreeCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 5

data    Three' a b
    =   Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)
  where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

type    ThreeIdentity' a b
    =   Three' a b
    ->  Bool

type    ThreeCompose' a b
    =   Three' a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretThreeIdentity' = ThreeIdentity' Int Int
type    ConcretThreeCompose'  = ThreeCompose' Int Int

testThreeFunctor' = do
    quickCheck (functorIdentity :: ConcretThreeIdentity')
    quickCheck (functorCompose  :: ConcretThreeCompose')

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 6

data    Four a b c d
    =   Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    =>  Arbitrary (Four a b c d)
  where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type    FourIdentity a b c d
    =   Four a b c d
    ->  Bool

type    FourCompose a b c d
    =   Four a b c d
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretFourIdentity = FourIdentity Int Int Int Int
type    ConcretFourCompose  = FourCompose Int Int Int Int

testFourFunctor = do
    quickCheck (functorIdentity :: ConcretFourIdentity)
    quickCheck (functorCompose  :: ConcretFourCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 7

data    Four' a b
    =   Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b)
  where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type    FourIdentity' a b
    =   Four' a b
    ->  Bool

type    FourCompose' a b
    =   Four' a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretFourIdentity' = FourIdentity' Int Int
type    ConcretFourCompose'  = FourCompose' Int Int

testFourFunctor' = do
    quickCheck (functorIdentity :: ConcretFourIdentity')
    quickCheck (functorCompose  :: ConcretFourCompose')

-- ------------------------------------------------------------------------- --
-- Chapter  16.10 — Instances of Func
-- Exercise 8
--
-- Can you implement one for this type? Why? Why not?
-- data Trivial = Trivial
--
-- No. Trivial has kind *, Functor requires kind * -> *.

-- ------------------------------------------------------------------------- --
-- Chapter  16.11 — Ignoring possibilities
-- Exercise       — Possibly
--
-- Write a Functor instance for a datatype identical to Maybe. We'll use our
-- own datatype because Maybe already has a Functor instance and we cannot
-- make a duplicate one.

data    Possibly a
    =   LolNope
    |   Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

instance (Arbitrary a) => Arbitrary (Possibly a)
  where
    arbitrary = frequency
        [ (1, return LolNope)
        , (3, Yeppers <$> arbitrary)
        ]

type    PossiblyIdentity a
    =   Possibly a
    ->  Bool

type    PossiblyCompose a
    =   Possibly a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretPossiblyIdentity = PossiblyIdentity Int
type    ConcretPossiblyCompose  = PossiblyCompose Int

testPossiblyFunctor = do
    quickCheck (functorIdentity :: ConcretPossiblyIdentity)
    quickCheck (functorCompose  :: ConcretPossiblyCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.11 — Ignoring possibilities
-- Exercise       — Short Exercise
--
-- Write a Functor instance for a datatype identical to Either. We'll use
-- our own datatype because Either has a Functor instance.

data    Sum a b
    =   First a
    |   Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First  a) = First a
    fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b)
  where
    arbitrary = frequency
        [ (1, First  <$> arbitrary)
        , (3, Second <$> arbitrary)
        ]

type    SumIdentity a b
    =   Sum a b
    ->  Bool

type    SumCompose a b
    =   Sum a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretSumIdentity = SumIdentity Int Int
type    ConcretSumCompose  = SumCompose Int Int

testSumFunctor = do
    quickCheck (functorIdentity :: ConcretSumIdentity)
    quickCheck (functorCompose  :: ConcretSumCompose)

-- ------------------------------------------------------------------------- --
-- Chapter  16.17 — Chapter Exercises
-- Exercise 1
--
-- Determine if a valid Functor can be written for the datatype provided.

-- ------------------------------------------------------------------------- --
--      1.

data    Bool' = IsFalse | IsTrue

--      No, kind of Bool is *.

-- ------------------------------------------------------------------------- --
--      2.

data    BoolAndSomethingElse a
    =   False' a
    |   True' a
    deriving (Eq, Show)

instance Functor (BoolAndSomethingElse) where
    fmap f (False' a) = False' (f a)
    fmap f (True'  a) = True'  (f a)

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a)
  where
    arbitrary = frequency
        [ (1, False' <$> arbitrary)
        , (1, True'  <$> arbitrary)
        ]

type    BoolAndSomethingElseIdentity a
    =   BoolAndSomethingElse a
    ->  Bool

type    BoolAndSomethingElseCompose a
    =   BoolAndSomethingElse a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretBoolAndSomethingElseIdentity = BoolAndSomethingElseIdentity Int
type    ConcretBoolAndSomethingElseCompose  = BoolAndSomethingElseCompose Int

testBoolAndSomethingElseFunctor = do
    quickCheck (functorIdentity :: ConcretBoolAndSomethingElseIdentity)
    quickCheck (functorCompose  :: ConcretBoolAndSomethingElseCompose)

-- ------------------------------------------------------------------------- --
--      3.

data    BoolAndMaybeSomethingElse a
    =   Falsish
    |   Truish a
    deriving (Eq, Show)

instance Functor (BoolAndMaybeSomethingElse) where
    fmap _ Falsish = Falsish
    fmap f (Truish  a) = Truish (f a)

instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a)
  where
    arbitrary = frequency
        [ (1, return Falsish)
        , (3, Truish  <$> arbitrary)
        ]

type    BoolAndMaybeSomethingElseIdentity a
    =   BoolAndMaybeSomethingElse a
    ->  Bool

type    BoolAndMaybeSomethingElseCompose a
    =   BoolAndMaybeSomethingElse a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretBoolAndMaybeSomethingElseIdentity = BoolAndMaybeSomethingElseIdentity Int
type    ConcretBoolAndMaybeSomethingElseCompose  = BoolAndMaybeSomethingElseCompose Int

testBoolAndMaybeSomethingElseFunctor = do
    quickCheck (functorIdentity :: ConcretBoolAndMaybeSomethingElseIdentity)
    quickCheck (functorCompose  :: ConcretBoolAndMaybeSomethingElseCompose)

-- ------------------------------------------------------------------------- --
--      4.  Use the kinds to guide you on this one, don't get too hung up on
--          the details.

newtype Mu f
    =   InF { outF :: f (Mu f) }

--      No, kind of Mu is (* -> *) -> *.

-- ------------------------------------------------------------------------- --
--      5.  Again, follow the kinds and ignore the unfamiliar parts.


data    D =
        D (Array Word Word) Int Int

--      No, kind of D is *.

-- ------------------------------------------------------------------------- --
-- Chapter  16.17 — Chapter Exercises
-- Exercise 2
--
-- Rearrange the arguments to the type constructor of the datatype so the
-- Functor instance works.

-- ------------------------------------------------------------------------- --
--      1.

--      Sum' a b
data    Sum' b a
    =   First' a
    |   Second' b

instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

-- ------------------------------------------------------------------------- --
--      2.

--      Company a b c
data    Company a c b
    =   DeepBlue a c
    |   Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- ------------------------------------------------------------------------- --
--      3.

--      More a b
data    More b a
    =   L a b a
    |   R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- ------------------------------------------------------------------------- --
-- Chapter  16.17 l— Chapter Exercises
-- Exercise 3
--
-- Write Functor instances for the following datatypes.

-- ------------------------------------------------------------------------- --
--      1.

data    Quant a b
    =   Finance
    |   Desk a
    |   Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b)
  where
    arbitrary = frequency
        [ (1, return Finance)
        , (2, Desk  <$> arbitrary)
        , (2, Bloor <$> arbitrary)
        ]

type    QuantIdentity a b
    =   Quant a b
    ->  Bool

type    QuantCompose a b
    =   Quant a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretQuantIdentity = QuantIdentity Int Int
type    ConcretQuantCompose  = QuantCompose Int Int

testQuantFunctor = do
    quickCheck (functorIdentity :: ConcretQuantIdentity)
    quickCheck (functorCompose  :: ConcretQuantCompose)

-- ------------------------------------------------------------------------- --
--      2.

data    K a b
    =   K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b)
  where
    arbitrary = K <$> arbitrary

type    KIdentity a b
    =   K a b
    ->  Bool

type    KCompose a b
    =   K a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretKIdentity = KIdentity Int Int
type    ConcretKCompose  = KCompose Int Int

testKFunctor = do
    quickCheck (functorIdentity :: ConcretKIdentity)
    quickCheck (functorCompose  :: ConcretKCompose)

-- ------------------------------------------------------------------------- --
--      3.

newtype Flip f a b
    =   Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K (f b))

instance (Arbitrary b) => Arbitrary (Flip K a b)
  where
    arbitrary = Flip <$> K <$> arbitrary

type    FlipKIdentity a b
    =   Flip K a b
    ->  Bool

type    FlipKCompose a b
    =   Flip K a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretFlipKIdentity = FlipKIdentity Int Int
type    ConcretFlipKCompose  = FlipKCompose Int Int

testFlipKFunctor = do
    quickCheck (functorIdentity :: ConcretFlipKIdentity)
    quickCheck (functorCompose  :: ConcretFlipKCompose)

-- ------------------------------------------------------------------------- --
--      4.

data    EvilGoateeConst a b
    =   GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b)
  where
    arbitrary = GoatyConst <$> arbitrary

type    EvilGoateeConstIdentity a b
    =   EvilGoateeConst a b
    ->  Bool

type    EvilGoateeConstCompose a b
    =   EvilGoateeConst a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretEvilGoateeConstIdentity = EvilGoateeConstIdentity Int Int
type    ConcretEvilGoateeConstCompose  = EvilGoateeConstCompose Int Int

testEvilGoateeConstFunctor = do
    quickCheck (functorIdentity :: ConcretEvilGoateeConstIdentity)
    quickCheck (functorCompose  :: ConcretEvilGoateeConstCompose)

-- ------------------------------------------------------------------------- --
--      5.  Do you need something extra to make the instance work?

data    LiftItOut f a
    =   LiftItOut (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a)
  where
    arbitrary = LiftItOut <$> arbitrary

type    LiftItOutIdentity f a
    =   LiftItOut f a
    ->  Bool

type    LiftItOutCompose f a
    =   LiftItOut f a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretLiftItOutIdentity = LiftItOutIdentity Maybe Int
type    ConcretLiftItOutCompose  = LiftItOutCompose Maybe Int

testLiftItOutFunctor = do
    quickCheck (functorIdentity :: ConcretLiftItOutIdentity)
    quickCheck (functorCompose  :: ConcretLiftItOutCompose)

-- ------------------------------------------------------------------------- --
--      6.

data    Parappa f g a
    =   DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a)
  where
    arbitrary = DaWrappa <$> arbitrary <*> arbitrary

type    ParappaIdentity f g a
    =   Parappa f g a
    ->  Bool

type    ParappaCompose f g a
    =   Parappa f g a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretParappaIdentity = ParappaIdentity Maybe Maybe Int
type    ConcretParappaCompose  = ParappaCompose Maybe Maybe Int

testParappaFunctor = do
    quickCheck (functorIdentity :: ConcretParappaIdentity)
    quickCheck (functorCompose  :: ConcretParappaCompose)

-- ------------------------------------------------------------------------- --
--      7.  Don't ask for more typeclass instances than you need. You can
--          let GHC tell you what to do.

data    IgnoreOne f g a b
    =   IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b)
  where
    arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

type    IgnoreOneIdentity f g a b
    =   IgnoreOne f g a b
    ->  Bool

type    IgnoreOneCompose f g a b
    =   IgnoreOne f g a b
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretIgnoreOneIdentity = IgnoreOneIdentity Maybe Maybe Int Int
type    ConcretIgnoreOneCompose  = IgnoreOneCompose Maybe Maybe Int Int

testIgnoreOneFunctor = do
    quickCheck (functorIdentity :: ConcretIgnoreOneIdentity)
    quickCheck (functorCompose  :: ConcretIgnoreOneCompose)

-- ------------------------------------------------------------------------- --
--      8.

data    Notorious g o a t
    =   Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t))
    => Arbitrary (Notorious g o a t)
  where
    arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

type    NotoriousIdentity g o a t
    =   Notorious g o a t
    ->  Bool

type    NotoriousCompose g o a t
    =   Notorious g o a t
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretNotoriousIdentity = NotoriousIdentity Maybe Int Int Int
type    ConcretNotoriousCompose  = NotoriousCompose Maybe Int Int Int

testNotoriousFunctor = do
    quickCheck (functorIdentity :: ConcretNotoriousIdentity)
    quickCheck (functorCompose  :: ConcretNotoriousCompose)

-- ------------------------------------------------------------------------- --
--      9.  You'll need to use recursion.

data    List a
    =   Nil
    |   Cons a (List a)
    deriving (Eq, Show)

instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a)
  where
    arbitrary = frequency
        [ (1, return Nil)
        , (3, Cons <$> arbitrary <*> arbitrary)
        ]

type    ListIdentity a
    =   List a
    ->  Bool

type    ListCompose a
    =   List a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretListIdentity = ListIdentity Int
type    ConcretListCompose  = ListCompose Int

testListFunctor = do
    quickCheck (functorIdentity :: ConcretListIdentity)
    quickCheck (functorCompose  :: ConcretListCompose)

-- ------------------------------------------------------------------------- --
--      10. A tree of goats forms a Goat-Lord, fearsome polycreature.

data    GoatLord a
    =   NoGoat
    |   OneGoat a
    |   MoreGoats (GoatLord a)
                  (GoatLord a)
                  (GoatLord a)
    deriving (Eq, Show)

instance Functor (GoatLord) where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

instance (Arbitrary a) => Arbitrary (GoatLord a)
  where
    arbitrary = frequency
        [ (1, return NoGoat)
        , (3, OneGoat <$> arbitrary)
        , (1, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary)
        ]

type    GoatLordIdentity a
    =   GoatLord a
    ->  Bool

type    GoatLordCompose a
    =   GoatLord a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretGoatLordIdentity = GoatLordIdentity Int
type    ConcretGoatLordCompose  = GoatLordCompose Int

testGoatLordFunctor = do
    quickCheck (functorIdentity :: ConcretGoatLordIdentity)
    quickCheck (functorCompose  :: ConcretGoatLordCompose)

-- ------------------------------------------------------------------------- --
--      11. You'll use an extra functor for this one, although your solution
--          might do it monomorphically without using fmap. Keep in mind
--          that you will probably not be able to validate this one in the
--          usual manner. Do your best to make it work.

data    TalkToMe a
    =   Halt
    |   Print String a
    |   Read (String -> a)

instance Functor (TalkToMe) where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read fa) = Read (fmap f fa)

instance (Show a) => Show (TalkToMe a)
  where
    show Halt = "Halt"
    show (Print s a) = "Print String=" ++ show s ++ " a=" ++ show a
    show (Read f) = "Read f(\"foo\")=" ++ show (f "foo")

instance (Eq a) => Eq (TalkToMe a)
  where
    Halt == Halt = True
    (Print s1 a1) == (Print s2 a2) = s1 == s2 && a1 == a2
    (Read f) == (Read g) = True
    _ == _ = False

instance (Arbitrary a) => Arbitrary (TalkToMe a)
  where
    arbitrary = frequency
        [ (1, return Halt)
        , (2, Print <$> arbitrary <*> arbitrary)
        , (2, Read <$> arbitrary)
        ]

type    TalkToMeIdentity a
    =   TalkToMe a
    ->  Bool

type    TalkToMeCompose a
    =   TalkToMe a
    ->  Fun Int Int
    ->  Fun Int Int
    ->  Bool

type    ConcretTalkToMeIdentity = TalkToMeIdentity Int
type    ConcretTalkToMeCompose  = TalkToMeCompose Int

testTalkToMeFunctor = do
    quickCheck (functorIdentity :: ConcretTalkToMeIdentity)
    quickCheck (functorCompose  :: ConcretTalkToMeCompose)
