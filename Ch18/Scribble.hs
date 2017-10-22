import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind x = join . fmap x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

  