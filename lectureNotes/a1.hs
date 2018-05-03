

data Exp = Num Double | Exp :+: Exp | Neg Exp | Exp :*: Exp | Inv Exp

eval :: Exp -> Double
eval (Num i) = i
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (Neg e) = - eval e
eval (Inv e) = 1 / eval e

e1, e2 :: Exp
e1 = Neg (Num 3 :+: (Num 4 :*: Inv (Num 2.4)))

e2 = Neg $ Inv $ Num 0
