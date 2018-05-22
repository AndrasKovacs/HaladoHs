
{-# language GADTs, PartialTypeSignatures, TypeFamilies, TypeInType,
    TypeOperators, ScopedTypeVariables, RebindableSyntax #-}
{-# options_ghc -fno-warn-partial-type-signatures #-}

import Prelude hiding (Monad(..))
import Data.Kind -- innen jön a Type

-- -- GADT (well-typed evaluation for embedded language (STLC))
-- -- well-typed interpreter

-- -- indexed state monad

-- ------------------------------------------------------------

-- -- Nem feltétlen típushelyes
-- -- data Tm = Lam String Tm
-- --         | App | Var String | Lit Int | Mul Tm Tm | Add Tm Tm

-- -- "Intrinsically typed" / "correct by construction"   syntax
-- -- embedding depth (shallow / deep)

-- -- shallow embedding: reuse most of the features of the host language
-- add = (+)
-- mul = (*)
-- var = id
-- lam = id
-- app = id
-- lit = id

-- -- idTm      = Lam "x" (Var "x")
-- idShallow = \x -> x

-- -- fooTm = Lam "x" $ Lam "y" $ Add (Var "x") (Var "y")
-- fooShallow = lam $ \x -> lam $ \y -> add (var x) (var y)

-- -- Correct by construction deep embedding
-- ------------------------------------------------------------

-- -- data List a = Cons a (List a) | Nil

-- -- felsoroljuk a konstruktorok típusait
-- -- (megadhatjuk a típuskonstruktor típusát)
-- -- data List :: Type -> Type where
-- --   Cons :: a -> List a -> List a
-- --   Nil  :: List a

-- data BoolOrInt :: Type -> Type where
--   Bool :: Bool -> BoolOrInt Bool
--   Int  :: Int  -> BoolOrInt Int

-- -- data IsTrue :: Bool -> Type where
-- --   IsTrue  :: IsTrue True

-- foo :: forall a. BoolOrInt a -> a
-- foo (Bool b) = b
-- foo (Int n)  = n

-- -- GADT: (dumbed down inductive type family)
-- ------------------------------------------------------------

-- -- {x : Int, y : Int} ⊢ x : Int

-- -- {Int, Bool} ⊢ 0 : Bool      -- Jobbról-balra: (de Bruijn index)
--                                -- Balról-jobbra: (de Bruijn level)

-- -- λ x y z . x -- nameful syntax
-- -- (λ λ λ . 2) -- db Index
-- -- (λ λ λ . 0) -- db Level

-- -- t :: Var as a :  annak a bizonyítása, hogy (a ∈ as)
-- --                  másik értelmezés: unáris szám (index), ami as-be mutat

-- -- data Var :: [Type] -> Type -> Type where
-- --   Here  :: Var (a ': as) a
-- --   There :: Var as a -> Var (b ': as) a

-- -- p1 :: Var [Int, Bool, ()] Int
-- -- p1 = Here

-- -- p2 :: Var [Int, Bool, ()] ()
-- -- p2 = There (There Here)

-- -- data Exp :: [Type] -> Type -> Type where
-- --   Var :: Var as a -> Exp as a
-- --   App :: Exp as (a -> b) -> Exp as a -> Exp as b
-- --   Lam :: Exp (a ': as) b -> Exp as (a -> b)

-- -- t1 :: Exp '[] (Int -> Bool -> Int)
-- -- t1 = Lam $ Lam $ Var (There Here)

-- -- data Env :: [Type] -> Type where
-- --   Nil  :: Env '[]
-- --   (:>) :: a -> Env as -> Env (a ': as)
-- -- infixr 5 :>

-- -- evalVar :: Var as a -> Env as -> a
-- -- evalVar Here      (t :> ts) = t
-- -- evalVar (There x) (_ :> ts) = evalVar x ts

-- -- eval :: Exp as a -> Env as -> a
-- -- eval (Var x)   env = evalVar x env
-- -- eval (App t u) env = eval t env (eval u env)
-- -- eval (Lam t)   env = \a -> eval t (a :> env)

-- ------------------------------------------------------------

-- data Ty = Base | Ty :=> Ty | TBool
-- infixr 3 :=>

-- data Var :: [Ty] -> Ty -> Type where
--   Here  :: Var (a ': as) a
--   There :: Var as a -> Var (b ': as) a

-- p1 :: Var [Base, Base :=> Base, Base] _
-- p1 = Here

-- p2 :: Var [Base, Base :=> Base, Base] _
-- p2 = There (There Here)

-- data Exp :: [Ty] -> Ty -> Type where
--   Var :: Var as a -> Exp as a
--   App :: Exp as (a :=> b) -> Exp as a -> Exp as b
--   Lam :: Exp (a ': as) b -> Exp as (a :=> b)

-- t1 :: Exp '[] (Base :=> Base)
-- t1 = Lam $ Var Here

-- -- t2 :: Exp '[] Base
-- -- t2 = _

-- type family EvalTy (a :: Ty) :: Type where
--   EvalTy Base      = Int
--   EvalTy (a :=> b) = EvalTy a -> EvalTy b

-- type family EvalCxt (as :: [Ty]) :: Type where
--   EvalCxt '[]       = ()
--   EvalCxt (a ': as) = (EvalTy a, EvalCxt as)

-- evalVar :: Var as a -> EvalCxt as -> EvalTy a
-- evalVar Here      (t, ts) = t
-- evalVar (There x) (_, ts) = evalVar x ts

-- evalExp :: Exp as a -> EvalCxt as -> EvalTy a
-- evalExp (Var x)   env = evalVar x env
-- evalExp (App t u) env = evalExp t env (evalExp u env)
-- evalExp (Lam t)   env = \a -> evalExp t (a, env)

-- ------------------------------------------------------------

infixl 1 >>=
infixl 1 >>

class IxMonad (m :: foo -> foo -> Type -> Type) where
  return :: a -> m i i a
  (>>=)  :: m i j a -> (a -> m j k b) -> m i k b

ma >> mb = ma >>= const mb

-- newtype State s a = State (s -> (a, s))

newtype IxState i o a = IxState {runState :: i -> (a, o)}

instance IxMonad IxState where
  return a = IxState $ \i -> (a, i)
  IxState ma >>= f = IxState $ \s ->
    let (b, s') = ma s
    in runState (f b) s'

put :: o -> IxState i o ()
put s = IxState $ \_ -> ((), s)

get :: IxState i i i
get = IxState $ \i -> (i, i)

modify :: (i -> o) -> IxState i o ()
modify f = do
  i <- get
  put (f i)

------------------------------------------------------------

data Door = Opened | Closed

data DoorProg (d1 :: Door) (d2 :: Door) :: Type -> Type where
  Return :: a -> DoorProg d1 d1 a
  Open   :: DoorProg Opened d2 a -> DoorProg Closed d2 a
  Close  :: DoorProg Closed d2 a -> DoorProg Opened d2 a

instance IxMonad DoorProg where
  return = Return
  Return a >>= f = f a
  Open p   >>= f = Open (p >>= f)
  Close p  >>= f = Close (p >>= f)

close :: DoorProg Opened Closed ()
close = Close (Return ())

open :: DoorProg Closed Opened ()
open = Open (Return ())

t1 :: DoorProg Opened Opened ()
t1 = do
  close
  open
  close
  open
  return ()

evalDoor :: DoorProg d1 d2 a -> (Bool -> (a, Bool))
evalDoor (Return a) b = (a, b)
evalDoor (Close dp) b = evalDoor dp (not b)
evalDoor (Open  dp) b = evalDoor dp (not b)

--   Close (Open (Return ()))
