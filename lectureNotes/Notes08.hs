{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# options_ghc -fwarn-incomplete-patterns #-}


import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (fix)
import Control.Monad.Except hiding (fix)
import Data.Char
import Data.Maybe
import Debug.Trace

-- parsec, attoparsec, megaparsec
type Parser a = StateT String (Either String) a
        -- String -> Either String (a, String)

runParser :: Parser a -> String -> Either String a
runParser p str = fst <$> runStateT p str

eof :: Parser ()
eof = do
  s <- get
  case s of
    [] -> pure ()
    _  -> empty

anyChar :: Parser Char
anyChar = do
  s <- get
  case s of
    c:cs -> c <$ put cs
    _    -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- anyChar
  if p c then pure c else empty

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

natural :: Parser Int
natural =
  snd . foldr (\n (i, acc) -> (i*10, acc + i*n)) (1, 0)
  <$> some digit

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x  = chainl1 p op <|> return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do{ x <- p; rest x }
                    where
                      rest x = do {  f <- op
                                   ; y <- p
                                   ; rest (f x y)
                                   }
                                <|> return x

data Exp =
  Lit Int | Add Exp Exp | Mul Exp Exp | Lam String Exp |
  App Exp Exp | Var String
  deriving Show

-- data Exp = Var String | Lam String Exp | App Exp Exp

-- rekurzió
-- minden lista, fa, szám, Bool (Church kódolás)

-- foldr f z [a, b, c] == f a (f b (f c z))
-- (\f z -> f a (f b (f c z)))
-- true  -- (\t f -> t) exp1 exp2    (if true then exp1 else exp2)
-- false -- (\t f -> f)


ws :: Parser ()
ws = () <$ many (satisfy isSpace)


lam :: Parser Exp
lam = do
  char 'λ' *> ws
  x <- some (satisfy isAlpha) <* ws
  char '.' *> ws
  e <- pExp
  pure (Lam x e)

apps :: Parser Exp
apps = chainl1 add (pure App)

-- olyan Exp, ami Add
add :: Parser Exp
add = chainl1 mul (Add <$ char '+' <* ws)

mul :: Parser Exp
mul = chainl1 atom (Mul <$ char '*' <* ws)

-- olyan Exp, ami mindig helyes önmagában
atom :: Parser Exp
atom =
      (char '(' *> ws *> pExp <* char ')' <* ws)
  <|> (Lit <$> natural <* ws)
  <|> (Var <$> (some (satisfy isAlpha) <* ws))

pExp :: Parser Exp
pExp = lam <|> apps

pExp' :: Parser Exp
pExp' = ws *> pExp <* eof


-- Írjunk hatékony eval-t :: Exp -> Val
------------------------------------------------------------

instance Show (Val -> Val) where show _ = "LAM"
data Val = VLam (Val -> Val) | VLit Int deriving Show

eval :: Exp -> [(String, Val)] -> Val
eval e env = case e of
  Var x   -> maybe (error "error1") id $ lookup x env
  App t u -> case eval t env of
    VLam t -> t (eval u env)
    _      -> error "error2"
  Lit n   -> VLit n
  Add t u -> case (eval t env, eval u env) of
                (VLit n, VLit m) -> VLit (n + m)
                _ -> error "error3"
  Mul t u -> case (eval t env, eval u env) of
                (VLit n, VLit m) -> VLit (n * m)
                _ -> error "error4"
  Lam x t -> VLam (\v -> eval t ((x, v):env))

eval0 :: Exp -> Val
eval0 exp = eval exp []

test :: String -> Either String Val
test str = eval0 <$> runParser pExp' str

-- fix :: String
-- fix = "(λ f. (λ x. f (x x)) (λ x . f (x x)))"

-- fixpoint combinator
fix :: (a -> a) -> a
fix f = trace "fix" (let x = f x in x)

--   trace "fix" (f (fix f))
-- --  let x = f x in x

fact' f n = case n of 0 -> 1 :: Integer; _ -> n * f (n - 1)









  -- x :: String
  -- t :: Exp
  -- env :: [(String, Val)]
  -- v :: Val



-- S -> S |
-- s = s <|> ...


-- many' :: Alternative f => f a -> f [a]
-- many' p = some' p <|> pure []

-- some' :: Alternative f => f a -> f [a]
-- some' p = (:) <$> p <*> many' p

-- choice :: Parser a -> Parser a -> Parser a
-- choice (StateT ma) (StateT ma') = StateT $ \s ->
--   either (const (ma' s)) Right (ma s)

-- heterogén lista
