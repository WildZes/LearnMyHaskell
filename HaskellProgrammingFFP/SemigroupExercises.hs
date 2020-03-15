--Chapter 15
--Monoid, Semigroup
--15.14 Chapter exercises
--Additional thanks:
--https://stackoverflow.com/questions/33642741/haskell-arbitrary-instance-of-higher-order-type

module SemigroupExercises where

import qualified Data.Monoid as M
import Data.Semigroup
import qualified Test.QuickCheck as Q

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Q.Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Q.Arbitrary a => Q.Arbitrary (Identity a) where
  arbitrary = do
    a <- Q.arbitrary
    return (Identity a)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Two a b) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c) => Q.Arbitrary (Three a b c) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    c <- Q.arbitrary
    return (Three a b c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four w1 x1 y1 z1) <> (Four w2 x2 y2 z2) = Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c, Q.Arbitrary d) => Q.Arbitrary (Four a b c d) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    c <- Q.arbitrary
    d <- Q.arbitrary
    return (Four a b c d)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Q.Arbitrary BoolConj where
  arbitrary = Q.frequency [ (1, return $ BoolConj True)
                        , (1, return $ BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Q.Arbitrary BoolDisj where
  arbitrary = Q.frequency [ (1, return $ BoolDisj True)
                        , (1, return $ BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd b <> _     = Snd b
  Fst _ <> Snd x = Snd x
  x     <> y     = y

-- 9
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine fx) <> (Combine fy) = Combine (fx <> fy)

-- 10
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp fx) <> (Comp fy) = Comp (fx . fy)

-- 11
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> _         = Failure a
  _         <> Failure a = Failure a
  first     <> _         = first

-- 12
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success a)) <> (AccumulateRight (Success b)) = AccumulateRight $ Success (a <> b)
  AccumulateRight first         <> AccumulateRight second        = AccumulateRight (first <> second)

-- 13
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success a)) <> (AccumulateBoth (Success b)) = AccumulateBoth $ Success (a <> b)
  (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure b)) = AccumulateBoth $ Failure (a <> b)
  AccumulateBoth first         <> AccumulateBoth second        = AccumulateBoth (first <> second)

main :: IO ()
main = do
  Q.quickCheck (semigroupAssoc :: TrivialAssoc)
  Q.quickCheck (semigroupAssoc :: IdentityAssoc String)
  Q.quickCheck (semigroupAssoc :: TwoAssoc (Product Int) (Sum Int))
  Q.quickCheck (semigroupAssoc :: ThreeAssoc String (Sum Int) [Int])
  Q.quickCheck (semigroupAssoc :: FourAssoc String (Sum Int) [String] [String])
  Q.quickCheck (semigroupAssoc :: BoolConjAssoc)
  Q.quickCheck (semigroupAssoc :: BoolDisjAssoc)
