module ListAppl where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Control.Applicative

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h $ take' (n - 1) t

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List a) -> List a -> List a
flatMap f as = concat' $ fmap f as

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs 

instance Monoid (List a) where
    mempty = Nil
    mappend = append

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
    
