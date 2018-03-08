{-# options_ghc -fwarn-incomplete-patterns #-}
{-# language RankNTypes, ScopedTypeVariables, TypeApplications #-}

import Data.List
import Data.Kind

-- Haskell, FP, kontextusba helyezése

-- FP

-- Rust (Mozilla), Swift (Apple)
   -- Klasszikus OOP kidobnak
   -- Olyan feature-öket, ADT, mintaillesztés, típusosztályok
   -- C# (LINQ), Java, C++
   -- Új webes FP nyelvek:
   -- Elm, Purescript, Elixir, Erlang, FRP könyvtárak, React

------------------------------------------------------------

-- Miért Haskell:
--   legtöbb könyv, dokumentáció, eszköz, infrastruktúra, library
--   iparban. Haxl: Facebook spamszűrés, Standard Chartered (3 millió sor)
-- Mire nem lehet használni: high-performance, real-time system
--                           grafikus motor, kereskedési rendszer
--                           FP és típuselmélet

-- Haskell ismétlés (ADT)
------------------------------------------------------------

-- import Prelude hiding (Bool(..))


-- enumeráció
data Bool' = True' | False' deriving Show

data Direction = Up | Down | Left' | Right' deriving Show

-- függvények enumeráción: mintaillesztés

not' :: Bool' -> Bool'
not' True'  = False'
not' _      = True'   -- _ illeszkedik mindenre

not'2 :: Bool' -> Bool'
not'2 b = case b of
  True'  -> False'
  False' -> True'

turnAround :: Direction -> Direction
turnAround Up    = Down
turnAround Down  = Up
turnAround Left'  = Right'
turnAround Right' = Left'

--
------------------------------------------------------------

data Foo = Con1 Int | Con2 Bool Bool deriving Show

-- fun1 :: Foo -> Foo
-- fun1 (Con1 n) = Con1 (n + 10)

fun2 :: Foo -> Maybe Foo
fun2 (Con1 n) = Just (Con1 (n + 10))
fun2 _        = Nothing

-- Paraméteres + rekurzív data
------------------------------------------------------------

-- data Maybe a = Just a | Nothing   -- legegyszerűűb hiba jelzésre szolgáló típus

data List a = Nil | Cons a (List a) deriving Show

-- Nagyon fontos dolog: type hole
map' :: forall a b. (a -> b) -> (List a -> List b)
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)

------------------------------------------------------------

-- template <class A>
-- .... identity
-- identity<A>(a)

id' :: forall a. a -> a
id' x = x

const' :: forall a b. a -> b -> a
const' a b = a

compose :: forall (a :: *) (b :: *) (c :: *). (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- dependent Haskell fogalmakba (nem érdemes megtanulni)
-- id'' :: forall (a :: *)(f :: a -> *)(b :: a). f b -> f b
-- id'' x = x

-- ADT mitől algebrai
------------------------------------------------------------



-- GHC.Generics

-- data Zero
-- data One = One
-- data Plus a b = In1 a | In2 b
-- data Mult a b = Mult a b
-- data Exp b a = Exp (a -> b)

-- Power set: A halmaz, A -> Bool

-- type Bool = Plus One One
-- data Foo = ... Plus  ... Plus (_ Mult _ Mult _ Mult (Exp _ (Exp _)))

data Nat = Zero | Suc Nat

-- | Nat | = 1 + | Nat |
-- | Nat | = 1 + 1 + 1 + 1 + 1 ........     --- mindig van megoldása

------------------------------------------------------------

-- foldl-t definiáljuk foldr segítségével
-- (pontosan egy foldr alkalmazásra lesz szükség, semmi más függvényt ne használjunk)

-- "lusta rekurzió"
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- "akkumulátoros rekurzió"
--  vég-rekurzív
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- fun1 (Con2 True b1) = undefined
-- fun1 (Con2 b1 b2) = Con2 b1 b2
