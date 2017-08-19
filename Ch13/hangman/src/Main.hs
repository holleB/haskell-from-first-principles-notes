module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete y (x:xs) = 
  if y == x then 
    xs 
  else 
    x: myDelete y xs

listSubtract :: (Eq a) => [a] -> [a] -> [a]
listSubtract = foldl (flip myDelete)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return (wl !! randomIndex)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (return Nothing) xs) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) chr = chr `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) chr = chr `elem` guessed 

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where 
    zipper guessed wordChar guessChar =
      if wordChar == guessed then Just wordChar else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess] 
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
    (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick something else!"
      return puzzle 
    (True, _) -> do
      putStrLn "This character was in the word,\
                \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)
      
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length (listSubtract guessed wordToGuess) > 9) $ do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess 
    

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $ do 
    putStrLn "You win!"
    exitSuccess 


runGame :: Puzzle -> IO () 
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle 
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame 
    _ -> putStrLn "Your guess must\
                  \ be a single character"

main :: IO () 
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word) 
  runGame puzzle