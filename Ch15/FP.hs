import Data.Monoid

newtype Mem s a = Mem {
  runMem :: s -> (a, s)
}

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem s1) (Mem s2) = Mem $ \s -> (
    mappend (fst (s1 s)) (fst (s2 s)), snd (s1 (snd (s2 s))) 
     )

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft ==  runMem f' 0
  print $ rmright == runMem f' 0
