module Main where

import ListAppl
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

myList :: List (String, String, Int)
myList = (Cons ("b", "w", 1) Nil)

main :: IO ()
main = quickBatch (applicative myList)
