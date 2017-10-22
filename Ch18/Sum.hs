import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers
import Test.QuickCheck

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure = Second
  First x <*> _  = First x
  Second f <*> s = fmap f s

instance Monad (Sum a) where
  return = pure
  First x >>= _  = First x
  Second x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq
    