{-# LANGUAGE RankNTypes #-}
{-# language MagicHash #-}

import Unsafe.Coerce
import GHC.Prim

-- laziness, strictness, performance, functional data structures (intro)
-- space leak etc.


-- 1. Strict by default programming: easier to reason about


-- Options for strictness
--   1. BangPatterns
--   2. seq
--   3. deepseq

-- lazy if then else
ite :: Bool -> a -> a -> a
ite True  t f = t
ite False t f = f

-- Basic concepts:
-- 1. thunk, weak head normal forms, whnf forcing

-- thunk: take arbitrary expression like "f x y z"
--        in general, function application expressions are
--        thunks at runtime, and only evaluated when forced
--        thunk: data structure: first field is a function pointer
--                               rest of the fields are free variables
--                               captured from program environment

-- thunk example:
f :: Int -> Int -> (Int, Int)
f x y = ((+) x y, (*) x y)
 -- example for free variables: x and y are free in g's definition
 -- g is called a "closure" in some prog. langs (like Javascript, Python)
 where g z = x + y

-- Implementation of closures:
-- Very similar to thunks:
-- Structure
closure :: Int -> (Int -> Int)
closure x =
  if x < 10 then id
            else (\y -> x + y)
{-# NOINLINE closure #-}

foo = (unsafeCoerce closure :: Int -> Int)
{-# NOINLINE foo #-}

closure2 :: Int# -> Int -> Int
closure2 x = undefined

main :: IO ()
main = do
  n <- read <$> getLine
  print $ foo n
  main

-- C++/Rust: no such struct
data IdFun = IdFun (forall a. a -> a)

-- C++ : forall (a :: *). a -> a  (memory layout of a can change in specific cases)
-- GHC : forall (a :: *). a -> a  (memory layout uniform)
-- id :: forall a. a -> a
-- id @a

-- Int
-- data Int = I# Int# -- unboxed Int#

-- runtime reprezentáció:

--   n :: Int
--   |
--   tag | unboxed 64 bit     (összesen: 2x64 bit)







-- (a -> b)
-- ((closure 10) 20 32 232 234 23423423) 42342 42342




-- when we force (f 10 20)
--      we get a pair of thunks
--      ((\x y -> x + y, x, y), (\x y -> x * y, x, y)

-- thunk is a tuple, first field is a pointer to function,
-- function body can only refer to arguments (and no local definitions
-- or variables)
-- ((\x y .... -> t),
