import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mappend mempty a == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mappend a mempty == a

-- -------------------------

data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mappend = (<>)
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
       
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- -------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mappend = (<>)
  mempty = Identity mempty

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
    
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mappend = (<>)
  mempty = Two mempty mempty

-- -------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Arbitrary BoolConj where
  arbitrary = oneof [ return (BoolConj True), return (BoolConj False) ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

-- -------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (arbitrary :: Gen Bool)
  
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

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

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine mempty

-- -------------------------

newtype Comp a = Comp { unComp :: a -> a }
instance Semigroup (Comp a) where
  Comp x <> Comp y = Comp (x . y)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp mempty
  
-- -------------------------
-- -------------------------

main :: IO () 
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)