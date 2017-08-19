import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO () 
palindrome = forever $ do
    rawLine <- getLine
    let line1 = filter isLetter $ map toLower rawLine in
        if line1 == reverse line1 then 
            putStrLn "It's a palindrome!" 
        else
        do
            putStrLn "Nope!"
            exitSuccess

type Name = String 
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty 
                   | AgeTooLow 
                   | PersonInvalidUnknown String 
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age 
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
       "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO () 
gimmePerson = do
    putStr "Input the name: "
    name <- getLine
    putStr "Input the age: "
    ageStr <- getLine
    let age = read ageStr in
        let person = mkPerson name age in
            case person of
                (Left reason) -> putStrLn $ "An error occured: " ++ show reason
                (Right p) -> putStrLn $ "Yay! Successfully got a person: " ++ show p
