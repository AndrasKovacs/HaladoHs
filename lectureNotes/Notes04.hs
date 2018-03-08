{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}


-- import Control.Concurrent.Async
import Control.Monad

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- -- Idris, Agda, Coq -ban legális a következő is:n
-- fmap id = id
-- fmap f . fmap g = fmap (f . g)

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' a) = Just' (f a)

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- Map key value     (Map :: * -> * -> *)
-- lookup :: Map key value -> key -> value

-- m :: Map key value
-- f :: value -> value'
-- map f m :: Map key value'

instance Functor' ((->) a) where
  fmap' = (.)

-- IO műveletek: szintén funktor
-- main :: IO ()
-- main =
--   a1
--   a2
--   a3

-- Async: aszinkron művelet: szintén funktor

-- Monad
------------------------------------------------------------

-- Functor => Applicative => Monad

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative f => Monad m where
--   pure  :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b


-- mellékhatásos programok:
   -- pl IO, kivételdobás, írható/olvasható állapot,
   -- valószínűségeloszlás
   -- nemdeterminisztikus
   -- logikai programozás monád

-- példák

list1 :: [Int]
list1 = do
  x <- [0..10]
  y <- [10..20]
  zs <- pure (100 :: Int)
  [0..3]
  pure (x + y)

list1' :: [Int]
list1' =
  [0..10] >>= \x ->
  [10..20] >>= \y ->
  [0..3] >>= \_ ->
  pure (x + y)

list2 :: [Int]
list2 = do
  x <- [0..10]
  y <- if even x then [0..10] else [10..20]
  pure (x + y)



-- instance Monad [] where
--   pure a      = [a]
--   lista >>= f = concatMap f lista

------------------------------------------------------------

-- instance Monad IO where  -- primitív

-- getLine :: String  -- primitív

-- gondok:
-- 1. lustaság miatt ez használhatatlan
--    ML, OCaml, F# : nem lusta, tehát ez nem használhatatlan

-- 2. Ha szigorú nyelvben vagyunk, és getLine :: String
--    probléma: nem tudjuk a típusból, hogy csinál-e mellékhatást

-- "tisztán" funkcionális: érdekes mellékhatásokat mind a típusban
--  lécci tüntessük fel. Előny: ha nincs típusban mellékhatás, akkor
--  tudjuk, hogy nincs mellékhatás.

-- hátrány:

-- bindIO, bindList, bindXYZ: uniform szintaxis, tudjunk
-- (Monad m =>) polimorf kódot is írni

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = pure []
mapM' f (a:as) = do
  a' <- f a
  as' <- mapM' f as
  pure (a':as')

mapM'_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM'_ = undefined

-- sequence :: Monad m => [m a] -> m [a]
-- sequence = undefined

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Tree where
  pure = return
  (<*>) = ap

instance Monad Tree where
  return = Leaf
  Leaf a   >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f)

-- filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM' p [] = pure []
-- filterM' p (a:as) = do
--   b <- [True, False]
--   if b then do
--     as <- filterM' p as
--     pure (a:as)
--   else
--     filterM' p as
