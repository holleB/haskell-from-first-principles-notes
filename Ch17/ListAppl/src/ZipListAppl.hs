module ZipListAppl where

import ListAppl hiding (append)
import qualified ListAppl
import Test.QuickCheck
import Test.QuickCheck.Checkers 

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys' 
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys 
                in take' 3000 l

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

append :: ZipList' a -> ZipList' a -> ZipList' a
append (ZipList' x) (ZipList' y) = ZipList' $ ListAppl.append x y

instance Applicative ZipList' where 
  pure x = ZipList' (pure x) `append` (pure x)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons h t)) = 
    (ZipList' (Cons (f h) Nil)) `append` ((ZipList' fs) <*> (ZipList' t))