{-# LANGUAGE UndecidableInstances #-}
{-# language TypeInType, GADTs, TypeOperators, StandaloneDeriving,
    TypeApplications, ConstraintKinds, AllowAmbiguousTypes,
    ScopedTypeVariables, RankNTypes, TypeFamilies #-}

import Data.Kind
import Data.List
import Data.Type.Equality
import Data.Proxy

-- infixr 4 :>
-- data HList :: [Type] -> Type where
--   HNil :: HList '[]
--   (:>) :: a -> HList as -> HList (a ': as)

-- -- class ToList (as :: [Type]) where
-- --   toList :: HList as -> [String]

-- -- instance ToList '[] where
-- --   toList HNil = []

-- -- instance (Show a, ToList as) => ToList (a ': as) where
-- --   toList (a :> as) = show a : toList as

-- -- instance ToList as => Show (HList as) where
-- --   show = ("["++) . (++"]") . intercalate ", " . toList

-- type Foo a b = (() :: Constraint)

-- type family AllC (c :: Type -> Constraint) (as :: [Type]) :: Constraint where
--   AllC c '[]       = ()
--   AllC c (a ': as) = (c a, AllC c as)

-- hmap :: forall c as. AllC c as => (forall x. c x => x -> x) -> HList as -> HList as
-- hmap f HNil      = HNil
-- hmap f (a :> as) = f a :> hmap @c f as

-- hmap' :: forall c as y. AllC c as => (forall x. c x => x -> y) -> HList as -> [y]
-- hmap' f HNil      = []
-- hmap' f (a :> as) = f a : hmap' @c f as

-- instance AllC Show as => Show (HList as) where
--   show = ("["++) . (++"]") . intercalate ", " . hmap' @Show show

-- type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
--   '[]       ++ ys = ys
--   (x ': xs) ++ ys = x ': (xs ++ ys)

-- happend :: forall as bs. HList as -> HList bs -> HList (as ++ bs)
-- happend HNil      ys = ys
-- happend (x :> xs) ys = x :> happend xs ys

------------------------------------------------------------


-- data Rec :: [(Symbol, Type)] -> Type where
--   Nil   :: Rec '[]
--   RCons :: forall s a as. Proxy s -> a -> Rec as -> Rec ( '(s , a) ': as)

-- type family AllC (c :: a -> Constraint) (as :: [a]) :: Constraint where
--   AllC c '[]       = ()
--   AllC c (a ': as) = (c a, AllC c as)

-- headTag :: Rec ( '(s , a) ': as) -> Proxy s
-- headTag _ = Proxy

-- hmap' :: forall (c :: (Symbol, Type) -> Constraint) as y. AllC c as
--    => (forall s x. c '(s, x) => x -> y) -> Rec as -> [y]
-- hmap' f Nil = []
-- hmap' f (RCons (s :: Proxy s') (a :: a') (as :: Rec as')) =
--   f @s' a : hmap' @c @as' @y f as

data Nat = Z | Suc Nat deriving Show

data SNat (n :: Nat) :: Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat (Suc n)

infixr :>
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 'Z a
  (:>) :: a -> Vec n a -> Vec (Suc n) a

deriving instance Show a => Show (Vec n a)

type family (+) (a :: Nat) (b :: Nat) :: Nat where
  Z     + b = b
  Suc a + b = Suc (a + b)

lemma :: SNat n -> Suc n :~: (n + (Suc Z))
lemma SZ = Refl
lemma (SS n) = case lemma n of
  Refl -> Refl

vappend :: Vec n a -> Vec m a -> Vec (n + m) a
vappend Nil       ys = ys
vappend (x :> xs) ys = x :> vappend xs ys

vLen :: Vec n a -> SNat n
vLen Nil       = SZ
vLen (a :> as) = SS (vLen as)

vreverse :: Vec n a -> Vec n a
vreverse Nil       = Nil
vreverse (a :> as) = case vreverse as of
  as' -> case lemma (vLen as) of Refl -> as' `vappend` (a :> Nil)



  -- vreverse a `vappend` (a :> Nil)



-- algebraic ornament (annotation + erasure on data types in general)
