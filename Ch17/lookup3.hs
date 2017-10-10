import Data.List (elemIndex)
-- import Control.Applicative

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max

maxed :: Maybe Int 
-- maxed = liftA2 max' x y
maxed = max' <$>  x <*> y