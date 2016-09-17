{-# LANGUAGE FlexibleInstances #-}

module WriteFunctors where

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
	fmap _ Finance = Finance
	fmap _ (Desk a) = Desk a
	fmap f (Bloor b) = Bloor (f b)

--data K a b = K a
--instance Functor K where
--	fmap f (K a) = K (f a)
--newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
--newtype K a b = K a

--instance Functor (Flip K a) where
--	fmap f (K a) = Flip K (f a) 


data EvilGoatConst a b = GoatConst b
instance Functor (EvilGoatConst a) where
	fmap f (GoatConst b) = GoatConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance (Functor f) => Functor (LiftItOut f) where
	fmap g (LiftItOut a) = LiftItOut (fmap g a) 

--data Parappa f g a = DaWrappa (f a) (g a)
--instance (Functor f, Functor g) => Functor (Parappa f g) where
--	fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap g a) 

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
	fmap f (IgnoreSomething a b) = IgnoreSomething a (fmap f b)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
	fmap f (Notorious a b c) = Notorious a b (fmap f c) 

data List a = Nil | Cons a (List a)
instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons a b) = Cons (f a) (fmap f b)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
	fmap _ NoGoat = NoGoat
	fmap f (OneGoat a) = OneGoat (f a)
	fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
	fmap _ Halt = Halt
	fmap f (Print str a) = Print str (f a)
	fmap f (Read a) = Read (fmap f a) 