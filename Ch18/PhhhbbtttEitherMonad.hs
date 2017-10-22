import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Prelude hiding (Left, Right)

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right x) = Right x
  fmap f (Left x) = Left (f x)

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  Right f <*> _ = Right f
  Left f <*> x = fmap f x
  
instance Monad (PhhhbbtttEither b) where
  return = pure
  Right x >>= _ = Right x
  Left x >>= f = f x
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

main :: IO ()
main = do
  let trigger :: PhhhbbtttEither String (String, String, Int); trigger = undefined 
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger