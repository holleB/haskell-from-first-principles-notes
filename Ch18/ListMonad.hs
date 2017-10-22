import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Control.Monad

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs 

instance Monoid (List a) where
    mempty = Nil
    mappend Nil xs = xs
    mappend xs Nil = xs
    mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)

instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x `mappend` (xs >>= f)

nonEmpty :: Gen a -> Gen (List a)
nonEmpty x = liftM2 Cons x (anyList x)

anyList :: Gen a -> Gen (List a)
anyList x = frequency [(1, pure Nil), (4, nonEmpty x)]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = anyList arbitrary

instance Eq a => EqProp (List a) where (=-=) = eq 
    
main :: IO ()
main = do
  let trigger :: List (String, String, Int); trigger = undefined 
  quickBatch $ functor trigger
  quickBatch $ monoid trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger  