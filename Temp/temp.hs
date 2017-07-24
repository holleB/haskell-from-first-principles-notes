{-# OPTIONS_GHC -Wall #-}
module Temp where

import Data.Char

extract :: Maybe (Maybe a) -> Maybe a
extract Nothing = Nothing
extract (Just a) = a

firstUpper :: String -> String
firstUpper [] = []
firstUpper (x : xs) = toUpper x : xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny predicate (x:xs)
    | predicate x    = True
    | otherwise = myAny predicate xs 

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs)
    | el == x   = True
    | otherwise = myElem el xs
    
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 el = myAny (el ==)

squish :: [[a]] -> [a]
squish [] = []
squish (xs: xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a: as) = f a ++ squishMap f as

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy _ [] = error "need at least one element"
myMaximumBy _ [a] = a
myMaximumBy comp (a1: a2: as)
    | comp a1 a2 == GT = myMaximumBy comp (a1 : as)
    | otherwise        = myMaximumBy comp (a2 : as)

myAny2 :: (a -> Bool) -> [a] -> Bool 
myAny2 f xs = foldr (\x b -> f x || b) False xs