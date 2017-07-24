fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibsLess = [x | x <- fibs, x < 100]

fact = scanl (*) 1 [1..]