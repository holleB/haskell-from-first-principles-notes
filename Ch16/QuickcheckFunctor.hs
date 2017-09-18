{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

{-# ANN module "HLint: ignore" #-}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = ((fmap g) . (fmap f)) x == fmap (g . f) x

functorCompose' :: (Functor f, Eq (f c)) => 
     f a
  -> Fun a b 
  -> Fun b c 
  -> Bool

functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

newtype Identity a = Identity a deriving (Eq, Show)
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Eq, Show)
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b = Two a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b1 <- arbitrary
      b2 <- arbitrary
      return $ Three' a b1 b2
instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)
type ThreeFC' = Three Int Int Int -> IntToInt -> IntToInt -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary 
      return $ Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

main :: IO () 
main = do
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck (functorCompose' :: PairFC)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck (functorCompose' :: ThreeFC')
  quickCheck (functorCompose' :: FourFC)

  
