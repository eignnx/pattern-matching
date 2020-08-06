module Parse (module Parse, module Control.Applicative) where

import Control.Applicative
import Data.Char

type PRes a = Maybe (a, String)
newtype Parser a = P (String -> PRes a)

parse :: Parser a -> String -> PRes a
parse (P p) input = p input

item :: Parser Char
item = P $ \input -> case input of
                          "" -> Nothing
                          (ch:rest) -> Just (ch, rest)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P $ \input -> case parse p input of
                       Nothing -> Nothing
                       Just (v, rest) -> Just (g v, rest)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P $ \input -> Just (v, input)

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pab <*> pa =
    P $ \input -> case parse pab input of
                       Nothing -> Nothing
                       Just (ab, rest) -> parse (fmap ab pa) rest

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \input -> case parse p input of
                               Nothing -> Nothing
                               Just (v, rest) -> parse (f v) rest

instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ \input -> Nothing

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \input -> case parse p1 input of
                                 Nothing -> parse p2 input
                                 res -> res

-- Derived Primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char ch = sat (== ch)

string :: String -> Parser String
string "" = return ""
string (ch:rest) = do char ch
                      string rest
                      return (ch:rest)

ident :: Parser String
ident = do first <- (lower <|> upper)
           rest <- many alphanum
           return (first:rest)

nat :: Parser Int
nat = do digits <- some digit
         return (read digits)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Handling Whitespace

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol sym = token (string sym)