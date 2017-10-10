module Main where

import ListAppl
import ZipListAppl
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

list1 :: List (String, String, Int)
list1 = (Cons ("b", "w", 1) Nil)

list2 :: ZipList' (String, String, Int)
list2 = ZipList' list1

main :: IO ()
main = do
  quickBatch (applicative list1)
  quickBatch (applicative list2)
