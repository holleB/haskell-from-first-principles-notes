import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- -------------------------

data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
       
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- -------------------------

newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdentAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

-- -------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two (Sum Rational) (Product Int) -> Two (Sum Rational) (Product Int) -> Two (Sum Rational) (Product Int) -> Bool
    
-- -------------------------

data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = Three String String String
               -> Three String String String
               -> Three String String String
               -> Bool 

-- -------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc = Four String String String String
              -> Four String String String String
              -> Four String String String String
              -> Bool 

-- -------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Arbitrary BoolConj where
  arbitrary = oneof [ return (BoolConj True), return (BoolConj False) ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- -------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (arbitrary :: Gen Bool)
  
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- -------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
  Fst _ <> Fst b = Fst b
  Fst _ <> Snd b = Snd b
  Snd a <> Fst _ = Snd a
  Snd a <> Snd _ = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [ fmap Fst arbitrary, fmap Snd arbitrary ]
  
type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

-- -------------------------

newtype Combine a b = Combine { unCombine :: a -> b }
instance Semigroup b => Semigroup (Combine a b) where
  Combine x <> Combine y = Combine (x <> y)

-- -------------------------

newtype Comp a = Comp { unComp :: a -> a }
instance Semigroup (Comp a) where
  Comp x <> Comp y = Comp (x . y)

-- -------------------------

data Validation a b = Failure a | Success b
    deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Failure a1 <> Failure a2 = Failure $ a1 <> a2
  Failure _ <> Success a = Success a
  Success a <> Failure _ = Success a
  Success a <> Success _ = Success a
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [ fmap Failure arbitrary, fmap Success arbitrary ]

type ValidationAssoc = Validation [Int] [Int] 
                    -> Validation [Int] [Int] 
                    -> Validation [Int] [Int]
                     -> Bool

-- -------------------------

newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Failure _) <> AccumulateRight f@(Failure _) = AccumulateRight f
  AccumulateRight s@(Success _) <> AccumulateRight (Failure _) = AccumulateRight s
  AccumulateRight (Failure _) <> AccumulateRight s@(Success _) = AccumulateRight s
  AccumulateRight (Success b1) <> AccumulateRight (Success b2) 
    = AccumulateRight (Success (b1 <> b2))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b)
  where arbitrary = fmap AccumulateRight arbitrary

type AccumulateRightAssoc = AccumulateRight [Int] [Int] 
                         -> AccumulateRight [Int] [Int]
                         -> AccumulateRight [Int] [Int]
                         -> Bool

-- -------------------------

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure a1) <> AccumulateBoth (Failure a2) = 
      AccumulateBoth $ Failure $ a1 <> a2
  AccumulateBoth s@(Success _) <> AccumulateBoth (Failure _) = AccumulateBoth s
  AccumulateBoth (Failure _) <> AccumulateBoth s@(Success _) = AccumulateBoth s
  AccumulateBoth (Success b1) <> AccumulateBoth (Success b2) = 
    AccumulateBoth (Success (b1 <> b2))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b)
  where arbitrary = fmap AccumulateBoth arbitrary

type AccumulateBothAssoc = AccumulateBoth [Int] [Int] 
                        -> AccumulateBoth [Int] [Int]
                        -> AccumulateBoth [Int] [Int]
                        -> Bool

-- -------------------------
-- -------------------------

main :: IO () 
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)