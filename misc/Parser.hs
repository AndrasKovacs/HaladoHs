{-# LANGUAGE FlexibleInstances #-}
{-# language LambdaCase #-}

-- Parser monád/kombinátorok
------------------------------------------------------------

import Control.Monad.State
import Control.Monad
import Control.Applicative hiding (some, many)
import Data.Monoid
import Data.String
import Data.Foldable
import Data.Char
import Debug.Trace

newtype Parser a = Parser
  {runParser :: String -> (Maybe a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser
    (\s -> let (res, s') = p s in (fmap f res, s'))

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser (\s -> (pure a, s))
  Parser p >>= f = Parser (\s ->
    let (res, s') = p s
    in case res of
         Nothing -> (Nothing, s')
         Just a  -> runParser (f a) s')

instance Alternative Parser where
  empty = Parser (\s -> (Nothing, s))
  Parser p <|> Parser q = Parser (\s ->
     let (res, s') = p s
     in case res of
          Nothing -> q s
          Just a  -> (Just a, s'))

anyChar :: Parser Char
anyChar = Parser (\case (c:s) -> (Just c, s); [] -> (Nothing, []))

option :: Parser a -> Parser (Maybe a)
option p = (Just <$> p) <|> pure Nothing

-- 0 vagy több
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- 1 vagy több
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

word :: String -> Parser String
word = mapM char

choose :: [Parser a] -> Parser a
choose = foldr (<|>) empty

eof :: Parser ()
eof = Parser (\case [] -> (Just (), []); s -> (Nothing, s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- anyChar
  if p c then pure c else empty

char :: Char -> Parser Char
char c = satisfy (==c)

pInt :: Parser Int
pInt = read <$> many1 (satisfy isDigit)

ws :: Parser ()
ws = () <$ many (satisfy isSpace)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest where
  rest x = do{ f <- op
             ; y <- p
             ; rest (f x y)
             } <|> pure x

runParser' :: Parser a -> String -> Maybe a
runParser' p s = fst $ runParser p s

------------------------------------------------------------

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
  deriving Show

add :: Parser Exp
add = chainl1 mul (Add <$ (char '+' <* ws))

mul :: Parser Exp
mul = chainl1 base (Mul <$ (char '*' <* ws))

base :: Parser Exp
base = (Lit <$> pInt <* ws) <|> (char '(' *> add <* (char ')' <* ws))

eval :: Exp -> Int
eval (Lit n) = n
eval (Add n m) = eval n + eval m
eval (Mul n m) = eval n * eval m
