import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> x = fmap f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x 

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

main :: IO ()
main = do
  let trigger :: Identity (String, String, Int); trigger = undefined 
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger  