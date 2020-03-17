module MonoidExercises where

import qualified SemigroupExercises as SgE
import qualified Data.Monoid as M
import Data.Semigroup
import qualified Test.QuickCheck as Q

monoidLeftCheck :: (Eq m, Monoid m) => m -> Bool
monoidLeftCheck a = mappend mempty a == a

monoidRightCheck :: (Eq m, Monoid m) => m -> Bool
monoidRightCheck a = mappend a mempty == a

-- 1
instance M.Monoid SgE.Trivial where
  mempty = SgE.Trivial
  mappend = (<>)

type TrivialLeftRight = SgE.Trivial -> Bool

-- 2
instance (Semigroup a, M.Monoid a) => M.Monoid (SgE.Identity a) where
  mempty = SgE.Identity mempty
  mappend = (<>)

type IdentityLeftRight a = SgE.Identity a -> Bool

-- 3
instance (Semigroup a, M.Monoid a, Semigroup b, M.Monoid b) => M.Monoid (SgE.Two a b) where
  mempty = SgE.Two mempty mempty
  mappend = (<>)

type TwoLeftRight a b = SgE.Two a b -> Bool

-- 4
instance M.Monoid SgE.BoolConj where
  mempty = SgE.BoolConj True
  mappend = (<>)

type ConjLeftRight = SgE.BoolConj -> Bool

-- 5
instance M.Monoid SgE.BoolDisj where
  mempty = SgE.BoolDisj False
  mappend = (<>)

type DisjLeftRight = SgE.BoolDisj -> Bool

-- 6
--With newtype and Semigroup instance taken from stranger's solution
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine fx) <> (Combine fy) = Combine (\x -> (fx x) <> (fy x))

instance (Semigroup b, M.Monoid b) => M.Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)
  mappend = (<>)

--quickCheck is taken from this repository, as well as newtype and Semigroup instance
--https://github.com/scarvalhojr/haskellbook/blob/master/chapter15/section15.15.hs
instance Show (Combine a b) where
  show _ = "Combine a b"

instance (Q.CoArbitrary a, Q.Arbitrary b) => Q.Arbitrary (Combine a b) where
  arbitrary = fmap Combine Q.arbitrary

combineSemigroupAssoc :: (Eq b, Show b, Semigroup b)
                      => a
                      -> Combine a b
                      -> Combine a b
                      -> Combine a b
                      -> Bool
combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

type CombineTest = Combine String [Int]

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

res1 :: Sum Int
res1 = unCombine (f <> g) $ 0

res2 :: Sum Int
res2 = unCombine (mappend f mempty) $ 1

-- 7 (hole task is taken from scarvalhojr gitHub)
newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

f' = Comp $ \(Sum n) -> Sum (n + 1)
g' = Comp $ \(Sum n) -> Sum (n - 1)

res1' :: Sum Int
res1' = unComp (f' <> g') $ Sum 0

res2' :: Sum Int
res2' = unComp (mappend f' mempty) $ Sum 0

-- 8 (this task was fully copypasted from scarvalhojr gitHub, by now I have no opportunity to understand the task)
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

  mappend (Mem f) (Mem g) = Mem (\x -> let (f1, x')  = f x
                                           (g1, x'') = g x'
                                        in (mappend f1 g1, x''))

f'' :: Mem Int String
f'' = Mem $ \s -> ("hi", s + 1)

g'' :: Mem Int String
g'' = Mem $ \s -> ("ho", s * 2)

main :: IO ()
main = do
  Q.quickCheck (monoidLeftCheck :: DisjLeftRight)
  Q.quickCheck (monoidRightCheck :: DisjLeftRight)
  Q.quickCheck (combineSemigroupAssoc :: String -> CombineTest -> CombineTest -> CombineTest -> Bool)
