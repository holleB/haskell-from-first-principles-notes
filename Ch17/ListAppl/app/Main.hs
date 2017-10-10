module Main where

import ListAppl

f :: List (Integer -> Integer)
f = Cons (+1) (Cons (*2) Nil)
v :: List Integer
v = Cons 1 (Cons 2 Nil)

main :: IO ()
main = putStrLn $ show $ f <*> v
