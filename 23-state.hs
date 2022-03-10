{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

-- ------------------------------------------------------------------------- --
-- Chapter  23    — State
-- ------------------------------------------------------------------------- --

import           Control.Arrow              ( first )

import           Test.Hspec                 ( hspec, describe, it, shouldBe )
import           Test.Hspec.QuickCheck      ( prop )

import           Test.QuickCheck
import           Test.QuickCheck.Gen.Unsafe ( promote )
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes    ( functor, applicative, monad )

-- ------------------------------------------------------------------------- --

-- Concret types used in functor, applicative and monad properties

concret :: m (String, Int, [Int])
concret = undefined

-- ------------------------------------------------------------------------- --
-- Chapter  23.06 — Write State for yourself
-- Exercise

newtype State s a
    =   State { runState :: s -> (a, s) }
    deriving (Show)

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ (first f) . g
--  fmap f (State g) = State $ \s ->
--      let (  ga, gs) = g s
--      in  (f ga, gs)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (a,)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*> (State g) = State $ \s ->
        let (ff   , fs) = f s
            (   ga, gs) = g fs
        in  (ff ga, gs)

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= g = State $ \s -> uncurry (runState . g) (f s)
--  (State f) >>= g = State $ \s ->
--      let (fa, fs) = f s
--      in  (runState (g fa)) fs

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (State s a) where
    arbitrary = State <$> promote (\s -> (,) <$> coarbitrary s arbitrary
                                             <*> coarbitrary s arbitrary)

instance (Show s, Arbitrary s , EqProp a, EqProp s) => EqProp (State s a) where
    (State sa) =-= (State sb) = (sa) =-= (sb)

testState :: IO ()
testState = hspec $ do
    describe "State (Laws)" $ do
        prop "Functor"     $ quickBatch $ functor     $ concret @(State String)
        prop "Applicative" $ quickBatch $ applicative $ concret @(State String)
        prop "Monad"       $ quickBatch $ monad       $ concret @(State String)

-- ------------------------------------------------------------------------- --
-- Chapter  23.08 — Chapter exercises
-- Exercise 1
--
-- Construct a State where the state is also the value you return.

get :: State s s
get = State $ \s -> (s, s)

testGet :: IO ()
testGet = hspec $ do
    describe "get" $ do
        it "constructs a State where the returned value is also the state" $
            property $ runState get "curry" `shouldBe` ("curry", "curry")

-- ------------------------------------------------------------------------- --
-- Chapter  23.08 — Chapter exercises
-- Exercise 2
--
-- Construct a State where the resulting state is the argument provided and
-- the value is defaulted to unit.

put :: s -> State s ()
put s = State $ const ((), s)

testPut :: IO ()
testPut = hspec $ do
    describe "put" $ do
        it "constructs a State with a constant given state and unit value" $
            property $ runState (put "blah") "woot" `shouldBe` ((), "blah")

-- ------------------------------------------------------------------------- --
-- Chapter  23.08 — Chapter exercises
-- Exercise 3
--
-- Run the State with s and get the state that results.

exec :: State s a -> s -> s
exec (State sa) = snd . sa

testExec :: IO ()
testExec = hspec $ do
    describe "exec" $ do
        it "runs the State and get the state that results" $
                 (property $ exec (put "wilma") "daphne" `shouldBe` "wilma")
            .&&. (property $ exec get "scooby papu" `shouldBe` "scooby papu")

-- ------------------------------------------------------------------------- --
-- Chapter  23.08 — Chapter exercises
-- Exercise 4
--
-- Run the State with s and get the value that results.

eval :: State s a -> s -> a
eval (State sa) = fst . sa

testEval :: IO ()
testEval = hspec $ do
    describe "eval" $ do
        it "runs the State and get the value that results" $
            property $ eval get "bunnicula" `shouldBe` "bunnicula"

-- ------------------------------------------------------------------------- --
-- Chapter  23.08 — Chapter exercises
-- Exercise 5
--
-- Write a function which applies a function to create a new State.

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

testModify :: IO ()
testModify = hspec $ do
    describe "modify" $ do
        it "applies a function to create a new State" $
            let m = modify (+1)
            in       (property $ runState       m  0 `shouldBe` ((), 1))
                .&&. (property $ runState (m >> m) 0 `shouldBe` ((), 2))
