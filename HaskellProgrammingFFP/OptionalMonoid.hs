--Chapter 15
--Monoid, Semigroup
--Write the Monoid instance for our Maybe type renamed to Optional.

module OptionalMonoid where

import Data.Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only one) (Only two) = Only (mappend one two)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
