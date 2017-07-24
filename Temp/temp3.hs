myPP :: [a] -> [a] -> [a]
myPP [] a2s = a2s
myPP (a1: a1s) a2s = a1: myPP a1s a2s

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x: xs) = x || myOr xs

myOr2 :: [Bool] -> Bool
myOr2 = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x: xs) = f x || myAny2 f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = myAny2 (el ==)

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 el = foldr (\x b -> el == x || b) False

myElem3 :: Eq a => a -> [a] -> Bool
myElem3 el = foldr ((||) . (el ==)) False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = go l []
  where go [] ys   = ys
        go (x: xs) ys = go xs (x:ys)
    
myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

myReverse3 :: [a] -> [a]
myReverse3 [] = []
myReverse3 (x:xs) = myReverse3 xs ++ [x]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x: xs) = f x : myMap f xs

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr ((:) . f) []

myMap3 :: (a -> b) -> [a] -> [b]
myMap3 f = foldr (\x b -> f x : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (a:as)
    | f a = a: myFilter f as
    | otherwise = myFilter f as

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\x b -> if f x then x: b else b) [] 

squish :: [[a]] -> [a]
squish [] = []
squish (l: ls) = l ++ squish ls

squish2 :: [[a]] -> [a]
squish2 = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a:as) = f a ++ squishMap f as

squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f = foldr ((++) . f) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy c (a1: a2: as)
    | c a1 a2 == GT = myMaximumBy c (a1: as)
    | otherwise = myMaximumBy c (a2: as)

myMaximumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy2 c (x: xs) = foldr (\y b -> if c y b == GT then y else b) x xs

myMaximumBy3 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy3 c l = foldr max' (head l) l
    where max' x y = if c x y == GT then x else y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy c = foldr1 min'
    where min' x y = if c x y == LT then x else y
