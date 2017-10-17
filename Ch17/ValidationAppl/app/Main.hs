module Main where

import Validation
  
success :: Validation [Errors] Int
success = Success (+1) <*> Success 1

failure :: Validation [Errors] Integer
failure = Success (+1) <*> Failure [StackOverflow] 

failure' :: Validation [Errors] Int
failure' = Failure [StackOverflow] <*> Success (+1)

failures :: Validation [Errors] Int
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]

main :: IO ()
main = do
  putStrLn $ show success
  putStrLn $ show failure
  putStrLn $ show failure'
  putStrLn $ show failures