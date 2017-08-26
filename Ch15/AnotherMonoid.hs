import Test.QuickCheck
import Data.Monoid

data Optional a =
  Nada 
  | Only a
  deriving (Eq, Show)

-- instance Monoid a => Monoid (Optional a) where
--   mempty                    = Nada
--   mappend Nada (Only a)     = Only a
--   mappend (Only a) Nada     = Only a
--   mappend (Only a) (Only b) = Only (a `mappend` b)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend fo@(First' (Only _)) _ = fo
  mappend _ fo@(First' (Only _)) = fo
  mappend (First' Nada) (First' Nada) = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = oneof [ return (First' Nada), fmap (First' . Only) arbitrary ]

firstMapped :: First' a -> First' a -> First' a
firstMapped = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)