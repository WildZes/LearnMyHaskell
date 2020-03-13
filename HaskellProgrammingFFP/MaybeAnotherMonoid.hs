--Chapter 15
--Monoid, Semigroup
--Write a Monoid instance for Maybe type which doesn’t require a Monoid
--for the contents. Reuse the Monoid law QuickCheck properties and
--use them to validate the instance.

module MaybeAnotherMonoid where

import OptionalMonoid
import Data.Monoid
import Test.QuickCheck

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x _             = x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [ (1, return $ First' Nada)
                        , (1, fmap (First' . Only) arbitrary)]

type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)


