module Main where
  
import Validation
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Validation [Errors] (Int, Int, Int))
  