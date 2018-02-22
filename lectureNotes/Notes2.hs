{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language TypeFamilies, GADTs #-}

-- Type class
------------------------------------------------------------

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f b as = foldr (\a g -> g . flip f a) id as b

------------------------------------------------------------




-- -- rekurzió
-- foldr cons nil (0:1:2:[])
--   = cons 0 (cons 1 (cons 2 nil))

data FinList a = Nil | Cons a !(FinList a)

-- data Tree a = Leaf a | Node (Tree a) (Tree a)

-- általánosan definiálhatnánk  az "induktív" típusokat, és
-- minden ilyen típushoz a rekurziót mint általánosított "foldr"
-- műveletet.

-- Type class
------------------------------------------------------------

-- data Foo a = Foo a

-- fooToString :: (a -> String) -> Foo a -> String
-- fooToString aToStr (Foo a) = aToStr a

-- foo :: Foo (Foo (Foo Int))
-- foo = undefined

data Pair a b = Pair a b

showPair :: (a -> String) -> (b -> String) -> Pair a b -> String
showPair f g (Pair a b) = "(" ++ f a ++ ", " ++ g b ++ ")"

-- kódgenerálás típusok struktúrája alapján
-- típuson való rekurzióval definiálni függvényt

class Show' a where
  show' :: a -> String

instance Show' Int where
  show' = show

instance Show' Bool where
  show' = show

instance (Show' a, Show' b) => Show' (Pair a b) where
  show' (Pair a b) = "(" ++ show' a ++ ", " ++ show' b ++ ")"

-- open universe assumption: typeclass: you can always add
-- new types with new instances, without changing behavior
-- of old code.
-- implicits:

class Eq' a where
  eq' :: a -> a -> Bool

instance Eq' a => Eq' [a] where
  eq' []     []     = True
  eq' (x:xs) (y:ys) = eq' x y && eq' xs ys
  eq' _      _      = False

-- [(Int, Bool, [Bool])]  (code synthesis)

-- Rossz class-ek alább:
------------------------------------------------------------

class Cast a b where
  cast :: a -> b

-- instance Cast Char Int where
--   cast = fromIntegral

class Default a where
  def :: a

-- Elméletileg megalapozott class-okat használjunk
-- elsősorban (kategóriaelméletileg + absztrakt algebrailag)
------------------------------------------------------------
--

class Functor' (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' f []     = []
  fmap' f (a:as) = f a : fmap' f as

instance Functor' Maybe where
  fmap' f Nothing  = Nothing
  fmap' f (Just a) = Just (f a)

instance Functor' ((->) a) where
  fmap' = (.)

-- class Num a where
--   fromIntegral ::

-- data Num a = Num
--   {plus :: a -> a -> a,
--    mult :: a -> a -> a,
--    fromInteger :: Integer -> a}

-- myFunction :: forall a. Eq a => a ->
-- myFunction a = ....
--   ......

-- union :: Ord k => Map k v -> Map k v -> Map k v
-- union a b = ...

-- class coherence: a programban bármely két instance azonos
-- típussal egyenlő (pontosan ugyanaz)

-- data ShowBox = forall a. Show a => ShowBox a

-- boxToString :: ShowBox -> String
-- boxToString (ShowBox a) = show a

-- instance Show ShowBox where
--   show (ShowBox a) = show a

data ShowBox' a = Show a => ShowBox' a

f :: forall a. Show a => ShowBox' a -> String
f (ShowBox' a) = show a
