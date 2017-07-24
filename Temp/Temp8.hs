module Temp8 where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:as) (y:bs)
    | x == y = isSubsequenceOf as bs
    | otherwise = isSubsequenceOf a bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map upperTuple . myWords
    where upperTuple :: String -> (String, String)
          upperTuple [] = ([], [])
          upperTuple a@(x:xs) = (a, toUpper x : xs)  
    
myWords :: String -> [String]
myWords "" = []
myWords s = case dropWhile isSpace s of
    "" -> []
    s' -> w: myWords s''
        where (w, s'') = mySpan (not . isSpace) s'

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([], [])
mySpan p xs@(x:xs')
    | p x = let (ys, zs) = mySpan p xs' in (x:ys, zs)
    | otherwise = ([], xs)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph p = concat $ map capitalizeWord (sentences p [])

sentences :: String -> [String] -> [String]
sentences s = go s []
    where go :: String -> String -> [String] -> [String]
          go [] [] a2 = reverse a2
          go [] a1 a2 = go [] [] (reverse a1 : a2)
          go (x1:x2:xs) a1 a2 = if x1 == '.' && x2 == ' '
                                    then go xs "" (reverse (x2:x1:a1) : a2)
                                    else go xs (x2:x1:a1) a2
          go (x1:xs) a1 a2 = go xs (x1:a1) a2

