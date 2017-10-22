import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

main :: IO ()
main = do
  let trigger :: Nope (String, String, Int); trigger = undefined 
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger  