data Exp = Num Int | Exp :+: Exp | Neg Exp

eval :: Exp -> Int
eval (Num i) = i
eval (e1 :+: e2) = eval e1 + eval e2
eval (Neg e) = - eval e

e1 :: Exp
e1 = Neg (Num 3 :+: Num 4)

