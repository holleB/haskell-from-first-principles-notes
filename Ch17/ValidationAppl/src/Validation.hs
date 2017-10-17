module Validation where

import Test.QuickCheck hiding (Failure, Success)
import Control.Applicative

data Validation e a = 
    Failure e
  | Success a 
  deriving (Eq, Show)

instance Functor (Validation e) where 
    fmap _ (Failure x) = Failure x
    fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
    pure = Success
    Success _ <*> Failure x = Failure x
    Failure x <*> Success _ = Failure x
    Failure x <*> Failure y = Failure (x `mappend` y)
    Success f <*> Success x = Success (f x)

data Errors = 
    DividedByZero
    | StackOverflow
    | MooglesChewedWires deriving (Eq, Show)

instance Arbitrary Errors where
    arbitrary = oneof [
        return DividedByZero
      , return StackOverflow
      , return MooglesChewedWires
      ]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = oneof [ fmap Failure arbitrary, fmap Success arbitrary ]
    