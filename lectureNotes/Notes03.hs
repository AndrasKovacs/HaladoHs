{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# language UnicodeSyntax #-}
{-# language DeriveFunctor, TypeOperators #-}
-- language pragmák (GHC User manualban listázva vannak)

import Data.Monoid
import Data.List
import Data.Function
-- import Data.Semigroup

-- import Prelude hiding (Monoid(..))
-- -- Monoid(..) : class-t is, metódusokat is

-- -- Monoid + polinomiális funktorok
-- -----------------------------------------------------

-- infixr 5 <>

-- class Semigroup a where
--   (<>) :: a -> a -> a
--   -- associative : ∀ x y z. (x <> y) <> z = (x <> (y <> z))

-- class Semigroup a => Monoid a where
--   mempty  :: a
--   -- left identity  : ∀ x. mempty <> x = x
--   -- right identity : ∀ x. x <> mempty = x

-- instance Semigroup [a] where
--   (<>) = (++)

-- instance Monoid [a] where
--   mempty = []

-- instance Semigroup a => Semigroup (Maybe a) where
--   Just x  <> Just y = Just (x <> y)
--   Nothing <> Just y = Just y
--   Just x  <> Nothing = Just x
--   Nothing <> Nothing = Nothing

-- instance Semigroup a => Monoid (Maybe a) where
--   mempty = Nothing

-- -- Mire jó ez a monoid dolog?
-- ------------------------------------------------------------

-- -- listát ragasztani
-- -- függvényt komponálni (két féleképpen is lehet függvény monoid)
-- -- Ordering monoid

-- instance Semigroup b => Semigroup (a -> b) where
--   f <> g = \x -> f x <> g x

-- instance Monoid b => Monoid (a -> b) where
--   mempty = \x -> mempty
foo :: [(Int, Int)]
foo = [(4, 5), (0, 3)]

-- combine ordering functions lexicographically:
foo' = sortBy ((compare `on` snd) <> (compare `on` fst)) foo

-- Text.Builder        Data.Text

newtype Endo' a = Endo' {runEndo :: a -> a}

instance Monoid (Endo' a) where
  Endo' f `mappend` Endo' g = Endo' (f . g)
  mempty = Endo' id

-- Alapvető
---------------

---------------------------------------------

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--  STM a  -- Functor

-- Elemi funktorok (legtöbb funktor felírható ezekkel)

newtype K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

newtype I a = I a

instance Functor I where
  fmap f (I a) = I (f a)

data (:*:) f g a = Product (f a) (g a)
  deriving (Functor) -- pointwise (,)

data (:+:) f g a = InL (f a) | InR (g a)
  deriving (Functor) -- pointwise Either

newtype Fix f = Fix (f (Fix f))

-- Fix f = f (Fix f)
--       = f (f (Fix f)
--       = f (f (f (Fix f)))
--       = ....

type List a = Fix (ListF a)

out :: Fix f -> f (Fix f)
out (Fix f) = f

---------------------------------------------------------

-- data NatF r = ZF | SF r
type NatF = K () :+: I
-- type ListF a = K () :+: (K a :*: I)

data ListF a r = NilF | ConsF a r

nil :: List a
nil = Fix NilF

cons :: a -> List a -> List a
cons a as = Fix (ConsF a as)

test :: List Int
test = cons 0 (cons 1 (cons 2 nil))

------------------------------------------------------------
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr = _

primListRec :: (a -> [a] -> b -> b) -> b -> [a] -> b
primListRec f z []     = z
primListRec f z (a:as) = f a as (primListRec f z as)

tails' :: [a] -> [[a]]
tails' = primListRec (\_ as acc -> as : acc) []

-- általános fold
cata :: Functor f => (f a -> a) -> Fix f -> a
cata processLayer (Fix f) =
  processLayer (fmap (cata processLayer) f)

-- általános primitív rekurzió
-- para :: ....
-- para

-- rekurziós sémák: (további kiterjesztése prim. rekurziónak)


-- ListF a fix pontja: ListF a (ListF a (ListF a (ListF a .....
-- f x = x

-- data List a = Nil | Cons a (List a)
-- data ListF a r =














-- amit még itt nem tudunk felírni: rekurzív típusok



















-- data Foo a = Foo

-- instance Functor Foo where
--   fmap f Foo = Foo

-- type Foo a = K ()
