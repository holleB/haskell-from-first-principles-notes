{-# LANGUAGE FlexibleInstances #-}
{-# ANN module "HLint: ignore" #-}

-- 1) No, wrong kindness

-- 2)
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' (f a)
  fmap f (False' a) = False' (f a)

-- 3)
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)
instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish (f a)
  fmap _ Falsish = Falsish

-- 4) No, wrong kindness
-- 5) No, wrong kindness

data Sum b a = First a | Second b
instance Functor (Sum e) where 
  fmap f (First a) = First (f a) 
  fmap f (Second b) = Second b

data Company a c b = 
    DeepBlue a c
  | Something b
instance Functor (Company e e') where 
  fmap f (Something b) = Something (f b) 
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b 
  deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a') 
  fmap f (R b a b') = R b (f a) b'
  
data Quant a b = 
    Finance
  | Desk a 
  | Bloor b
instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K a b = K a
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor x => Functor (LiftItOut x) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor x, Functor y) => Functor (Parappa x y) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a ) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
  
data List a = Nil | Cons a (List a) deriving Show
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats gla1 gla2 gla3) =
    MoreGoats (fmap f gla1) (fmap f gla2) (fmap f gla3)

data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read $ f . sa