{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language LambdaCase, ApplicativeDo #-}

import Data.Char
import Control.Applicative
import Control.Monad

-- State monad:

data Pair a b = Pair {first :: a, second :: b} deriving (Show)

--- x :: Pair a b            ( pointer to Pair a b )
---  |
--- tag | a | b              ( 3 machine words)

data One a = One a

--- x :: One a
--- |
--- tag | a                  (2 words)

-- newtype One a = One a
-- x :: One a
-- newtype State s a = State {runState ::

-- increment :: State Int ()
-- increment = do
--   n <- get
--   put (n + 1)

-- instance Functor ((->) s)

--      (a -> b) -> (s -> a) -> (s -> b)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State ma) = State (\s -> let (a, s') = ma s in (f a, s'))

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

-- put :: s -> State s ()
-- put s = undefined

-- get :: State s s
-- get = undefined

-- foo :: State Int Int
-- foo = do
--   a <- get
--   b <- get
--   pure (1 + b)

-- foo :: State Int Int
-- foo = (+) <$> get <*> get

-- ap :: Monad m => m (a -> m (a -> b)) -> m a -> m b
-- ap mf ma = do
--   f <- mf
--   a <- ma
--   return (f a)

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State ma >>= f = State $ \s ->
    let (a, s') = ma s
        State mb = f a
    in mb s'

    -- f :: a -> State s b
    -- f a :: State s b
    -- runState (f a) :: s -> (b, s)
    -- runState (f a) s' :: (b, s)

-- replace state with a give s value
put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

----------------------------------------

increment :: State Int ()
increment = modify (+1)

----------------------------------------

-- -- számozzuk be a leveleket balról jobbra mélységi bejárási
-- -- sorrendben
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving (Show)

-- label :: Tree a -> Tree Int
-- label t = evalState (label' t) 0

-- label' :: Tree a -> State Int (Tree Int)
-- label' (Leaf a)   = Leaf <$> (get <* modify (+1))
-- label' (Node l r) = Node <$> label' l <*> label' r

-- label' :: Tree a -> Int -> (Tree Int, Int)
-- label' (Leaf a) = do
--   i <- get
--   put (i + 1)
--   pure (Leaf i)
-- label' (Node l r) = (do
--   l <- label' l
--   r <- label' r
--   pure (Node l r)

-- myTree = Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))

-- Idiomatic solution
------------------------------------------------------------

-- számozzuk be a leveleket balról jobbra mélységi bejárási
-- sorrendben
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

label :: Tree a -> Tree Int
label t = evalState (mapM (\_ -> get <* modify (+1)) t) 0

myTree = Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))

------------------------------------------------------------

label'' :: Tree a -> Int -> (Tree Int, Int)
label'' (Leaf a) i = (Leaf i, i + 1)
label'' (Node l r) i =
  let (l', i') = label'' l i
      (r', i'') = label'' r i'
  in (Node l' r', i'')

-- [], State, Maybe, Either, IO, MonadTrans
------------------------------------------------------------
