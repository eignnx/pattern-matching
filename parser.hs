module Parser (module Parser) where

import Parse (Parser, integer, identifier, char, parse, symbol)
import Ast
import Control.Applicative
import Data.Maybe (fromJust)
import Data.List (uncons)

atomicExpr :: Parser Expr
atomicExpr = Num <$> integer <|> Sym <$> identifier

expr :: Parser Expr
expr = (do hd <- atomicExpr
           argGroups <- some arguments
           let (ag:ags) = argGroups
           return $ foldl Cons (Cons hd ag) ags)
      <|> atomicExpr

arguments :: Parser [Expr]
arguments = symbol "[" *> separatedList (symbol ",") expr <* symbol "]"

pattern :: Parser Patt
pattern = num <|> identHeaded <|> sym
  where
    num = (NumP <$> integer)
    identHeaded = do ident <- identifier
                     partial <- varIdentRest <|> consIdentRest
                     return $ partial ident
    sym = SymP <$> identifier

consPatternRest :: Parser (Patt -> Patt)
consPatternRest =
  do _ <- symbol "["
     args <- separatedList (symbol ",") pattern
     _ <- symbol "]"
     return $ \hd -> ConsP hd args

consIdentRest :: Parser (String -> Patt)
consIdentRest =
  do partial <- consPatternRest
     return $ \ident -> partial (SymP ident)

varIdentRest :: Parser (String -> Patt)
varIdentRest =
  do _ <- symbol "_"
     return $ \ident -> VarP ident

separatedNonemptyList :: Parser sep -> Parser a -> Parser [a]
separatedNonemptyList sep val = postfixComma <|> infixComma
  where
    infixComma = (:) <$> val <*> (many (sep *> val))
    postfixComma = infixComma <* sep

separatedList :: Parser sep -> Parser a -> Parser [a]
separatedList sep val =
  separatedNonemptyList sep val <|> pure []

rule :: Parser Rule
rule = do p <- pattern
          _ <- symbol ":="
          e <- expr
          return $ SetDelayed p e

stmt :: Parser Stmt
stmt = Rule <$> rule

parseAll :: Show a => Parser a -> String -> Either String a
parseAll p i =
  case parse p i of
       Just (x, "") -> Right x
       Just (x, rest) -> Left $ "parsed " ++ (showsPrec 100 x) "" ++ ", but input " ++ show rest ++ " remains!"
       Nothing -> Left "reached eof but couldn't successfully parse!"

printParse :: ToDisp a => Show a => Parser a -> String -> IO ()
printParse p i =
  case parseAll p i of 
       Right x -> print (toDisp x)
       Left err -> putStrLn err

parsePanic :: Show a => Parser a -> String -> a
parsePanic p i =
  case parseAll p i of
    Right x -> x
    Left err -> error err