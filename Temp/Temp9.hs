module Temp9 where

import Data.Char

type Digit = Char
type Presses = Int

data DaPhone = DaPhone [Button] deriving (Show)
data Button = Button Digit String deriving (Show)

daPhone :: DaPhone
daPhone = DaPhone [Button '1' "", Button '2' "abc", Button '3' "def",
                   Button '4' "ghi", Button '5' "jkl", Button '6' "mno",
                   Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz",
                   Button '*' "^", Button '0' "+ ", Button '#' ".,"]

convo :: [String] 
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

index :: (Eq a) => a -> [a] -> Int
index el = (1+) . length . takeWhile (/= el)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps (DaPhone (Button key chars : xs)) character 
    | elem character chars = [(key, 1 + index character chars)]
    | elem (toLower character) chars = [('*', 1), (key, 1 + index (toLower character) chars)]
    | otherwise = reverseTaps (DaPhone xs) character

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldr (\x b -> reverseTaps phone x ++ b) [] 

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- mostPopularLetter :: String -> Char
-- mostPopularLetter = groupBy . cellPhonesDead daPhone 

data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer 
eval (Lit int) = int
eval (Add left right) = eval left + eval right

printExpr :: Expr -> String 
printExpr (Lit int) = show int
printExpr (Add left right) = printExpr left ++ " + " ++ printExpr right