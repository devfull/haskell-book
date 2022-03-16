{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeApplications           #-}

-- ------------------------------------------------------------------------- --
-- Chapter  25    — Composing Types
-- ------------------------------------------------------------------------- --

import           Control.Applicative        ( liftA2 )

import           Test.Hspec
import           Test.Hspec.QuickCheck      ( prop )

import           Test.QuickCheck     hiding ( Success, Failure )
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes    ( functor
                                            , applicative
                                            , foldable
                                            , traversable
                                            )

-- ------------------------------------------------------------------------- --

-- Concret types used in functor, applicative and monad properties

concret :: m (String, Int, [Int])
concret = undefined

-- Concret types used in foldable properties

concretFoldable :: Foldable f => f (String, Int, [Int], Int, Int)
concretFoldable = undefined

-- ------------------------------------------------------------------------- --
-- Chapter  25.04 — Twinplicative
-- Exercise

newtype Compose f g a
    =   Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgh) <*> (Compose fga) = Compose $ liftA2 (<*>) fgh fga
--  (Compose fgh) <*> (Compose fga) = Compose $ ((<*>) <$> fgh) <*> fga

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Compose Instances
-- Exercise 1
--
-- Write the Compose Foldable instance.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> (Compose f g a) -> m
    foldMap h (Compose fga) = (foldMap . foldMap) h fga

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Compose Instances
-- Exercise 2
--
-- Write the Compose Traversable instance.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse h (Compose fga) = Compose <$> (traverse . traverse) h fga

-- ------------------------------------------------------------------------- --

instance (Arbitrary (f (g a))) => Arbitrary (Compose f g a)
  where
    arbitrary = Compose <$> arbitrary

instance (EqProp (f (g a))) => EqProp (Compose f g a) where
    (Compose fga) =-= (Compose fga') = fga =-= fga'

testCompose :: IO ()
testCompose = hspec $ do
    describe "Compose (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret         @(Compose Maybe Maybe)
        prop "Applicative" $ quickBatch $ applicative $ concret         @(Compose Maybe Maybe)
        prop "Foldable"    $ quickBatch $ foldable    $ concretFoldable @(Compose Maybe Maybe)
        prop "Traversable" $ quickBatch $ traversable $ concret         @(Compose Maybe Maybe)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 1
--
-- Write Bifunctor instances for the following types.

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data    Deux a b
    =   Deux a b

instance Bifunctor (Deux) where
    bimap :: (a -> a') -> (b -> b') -> Deux a b -> Deux a' b'
    bimap f g (Deux a b) = Deux (f a) (g b)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 2

data    Const a b
    =   Const a

instance Bifunctor (Const) where
    bimap :: (a -> a') -> (b -> b') -> Const a b -> Const a' b'
    bimap f _ (Const a) = Const (f a)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 3

data    Drei a b c
    =   Drei a b c

instance Bifunctor (Drei a) where
    bimap :: (b -> b') -> (c -> c') -> Drei a b c -> Drei a b' c'
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 4

data    SuperDrei a b c
    =   SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap :: (b -> b') -> (c -> c') -> SuperDrei a b c -> SuperDrei a b' c'
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 5

data    SemiDrei a b c
    =   SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap :: (b -> b') -> (c -> c') -> SemiDrei a b c -> SemiDrei a b' c'
    bimap _ _ (SemiDrei a) = SemiDrei a

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 6

data    Quadriceps a b c d
    =   Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap :: (c -> c') -> (d -> d') -> Quadriceps a b c d -> Quadriceps a b c' d'
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- ------------------------------------------------------------------------- --
-- Chapter  25.06 — Bifunctor
-- Exercise 7

instance Bifunctor (Either) where
    bimap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
    bimap f _ (Left  a) = Left  (f a)
    bimap _ g (Right b) = Right (g b)
