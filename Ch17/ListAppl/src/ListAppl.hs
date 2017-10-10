module ListAppl where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Control.Applicative

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs 

instance Monoid (List a) where
    mempty = Nil
    mappend Nil xs = xs
    mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Applicative List where 
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)
    
nonEmpty :: Gen a -> Gen (List a)
nonEmpty x = liftA2 Cons x (anyList x)

anyList :: Gen a -> Gen (List a)
anyList x = frequency [(1, pure Nil), (4, nonEmpty x)]

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = anyList arbitrary

instance Eq a => EqProp (List a) where (=-=) = eq 
    
