
import Control.Monad.State

data Tree a = Tree a [Tree a] deriving Show

-- 1. implementáld a `Functor` instance-ot `Tree`-hez.

-- 2. definiálj egy `(* -> *)` típuskonstruktort `data`-val, amire nem létezik `Functor` instance.

-- 3. `State` monad felhasználásával írj egy `label :: Tree a -> Tree (a, Int)` függvényt, 
--     ami 0-tól kezdve preorder sorrendben beszámozza egy `Tree`-ben az `a` adatokat.

-- Példa a működésre: `label (Tree () [Tree () [], Tree () []])) == Tree ((), 0) [Tree ((), 1) [], Tree ((), 2) []]`


data Exp = Int Int | Bool Bool | And Exp Exp | Eq Exp Exp | Add Exp Exp

-- 4. A fenti `Exp` típusra gondoljunk, mint `Bool` és `Int` típusú kifejezések fájára.
--    Az `Eq` két azonos típusú kifejezés egyenlőségvizsgálatát reprezentálja.

-- Írj `eval :: Exp -> Maybe (Either Int Bool)` függvényt `Maybe` monád segítségével.
-- Legyen `Just (Left n)` az eredmény, ha az input kiértékelhető `Int`-re, és
-- `Just (Right b)`, ha az input kiértékelhető `Bool`-ra, egyébként `Nothing`.

-- 5. Extra: írd meg a `label` függvényt csak `Ȧpplicative` függvények használatával. 
-- Tipp: a `modify` függvényt érdemes használni.
