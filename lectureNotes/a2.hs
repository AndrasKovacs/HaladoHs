{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Monad

data Nat = Zero | Succ Nat

safeNeg :: Nat -> Nat -> Maybe Nat
m      `safeNeg` Zero   = Just m
Zero   `safeNeg` _      = Nothing
Succ m `safeNeg` Succ n = m `safeNeg` n

instance Num Nat where
  Zero + n = n
  Succ m + n = Succ (m + n)
  Zero * n = Zero
  Succ m * n = n + (m * n)
  m - n = (\(Just k) -> k) $ m `safeNeg` n
  abs = id
  signum Zero = Zero
  signum (Succ _) = Succ Zero
  fromInteger n | n == 0 = Zero
                | n > 0  = Succ (fromInteger (n-1))

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

instance Show Nat where
  show n = show (toInt n) ++ "N"

data Exp = Num Nat | Exp :+: Exp | Exp :-: Exp

eval :: Exp -> Maybe Nat
eval (Num i) = Just i
eval (e1 :+: e2) = ((+) <$> eval e1) <*> eval e2

-- eval e2 :: Maybe Nat
-- <*> :: Maybe (Nat -> Nat) -> Maybe Nat -> Maybe Nat
-- (+) :: Nat -> (Nat -> Nat)
-- eval e1 :: Maybe Nat
-- <$> :: (Nat -> (Nat -> Nat)) -> Maybe Nat -> Maybe (Nat -> Nat)
-- (+) <$> eval e1 :: Maybe (Nat -> Nat)
-- eval (e1 :-: e2) = join $ liftA2 safeNeg (eval e1) (eval e2)

eval (e1 :-: e2) = join (safeNeg <$> eval e1 <*> eval e2)

e1, e2, e3 :: Exp
e1 = Num 3 :-: Num 4
e2 = Num 3 :-: Num 2
e3 = Num 3 :+: Num 5

join' :: Monad m => m (m c) -> m c
join' x = x >>= id

-- a =
-- b = c


-- fmap :: Functor f => (a -> b) -> (f a -> f b)
-- fmap id = id
-- fmap (g . f) = fmap g . fmap f

data Maybe' a where
  Nothing' :: Maybe' a
  Just'    :: a -> Maybe' a

  deriving Show

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' a) = Nothing'
