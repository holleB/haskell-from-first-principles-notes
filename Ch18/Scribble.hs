import Control.Monad 

bind :: Monad m => (a -> m b) -> m a -> m b
bind x = join . fmap x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- from Chapter Exercises
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = 
  let x' = (fmap (\l -> [l]) (f x)) 
      xs' = meh xs f
  in (fmap (++) x') <*> xs'

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id