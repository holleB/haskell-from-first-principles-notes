module Ch11 where

notThe :: String -> Maybe String 
notThe "the" = Nothing
notThe xs = Just xs

replaceThe :: String -> String
replaceThe = unwords . map (nothingToA . notThe) . words
    where nothingToA :: Maybe String -> String
          nothingToA (Just str) = str
          nothingToA Nothing = "a"

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
    where go :: Integer -> [String] -> Integer
          go acc (w1:w2:ws) = if w1 == "the" && isVowel (head w2)
                                then go (acc + 1) ws
                                else go acc ws
          go acc [_] = acc
          go acc [] = acc

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (not . isVowel)

newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word' 
mkWord xs = if countVowels xs > countConsonants xs
        then Just (Word' xs)
        else Nothing

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just (go i)
  where go 0 = Zero
        go i' = Succ (go (i' - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMayBe :: a -> Maybe a -> a
fromMayBe a Nothing = a
fromMayBe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe (a:_) = Just a
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:mas) = a:catMaybes mas
catMaybes (Nothing:mas) = catMaybes mas

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
    where f :: Maybe a -> Maybe [a] -> Maybe [a]
          f Nothing _ = Nothing
          f _ Nothing = Nothing
          f (Just a) (Just b) = Just (a: b)

lefts' :: [Either a b] -> [a]
lefts' = foldr f [] 
    where f :: Either a b -> [a] -> [a]
          f (Left a) b = a:b
          f _ b = b

rights' :: [Either a b] -> [b]
rights' = foldr f [] 
    where f :: Either a b -> [b] -> [b]
          f (Right b) bs = b:bs
          f _ bs = bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
    where f :: Either a b -> ([a], [b]) -> ([a], [b])
          f (Left a) (as, bs) = (a:as, bs)
          f (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f b = case f b of
    Just (current, next) -> current:myUnfoldr f next
    Nothing                 -> []

betterIterate :: (a -> a) -> a -> [a] 
betterIterate f = myUnfoldr (\y -> Just(y, f y)) 

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (b -> Maybe (b,a,b)) -> b -> BinaryTree a
unfold f b = case f b of
    Just (nextLeft, current, nextRight) -> Node (unfold f nextLeft) current (unfold f nextRight)
    Nothing                 -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild limit = unfold f 0
    where f :: Integer -> Maybe (Integer, Integer, Integer)
          f i
            | i == limit = Nothing
            | otherwise = Just (i + 1, i, i +1)