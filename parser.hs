module Parser (module Parser) where

import Parse (Parser, integer, identifier, char, parse, symbol)
import Ast
import Control.Applicative

atomicExpr :: Parser Expr
atomicExpr = Num <$> integer <|> Sym <$> identifier

expr :: Parser Expr
expr = (do hd <- atomicExpr
           argGroups <- some arguments
           let ag1 = head argGroups
           let ags = tail argGroups
           return $ foldl Cons (Cons hd ag1) ags)
      <|> atomicExpr

arguments :: Parser [Expr]
arguments =
  do _ <- symbol "["
     args <- separatedList (symbol ",") expr
     _ <- symbol "]"
     return args

consExprRest :: Parser (Expr -> Expr)
consExprRest =
  do _ <- symbol "["
     args <- separatedList (symbol ",") expr
     _ <- symbol "]"
     return $ \hd -> Cons hd args

pattern :: Parser Patt
pattern = num <|> identHeaded <|> sym where
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
separatedNonemptyList sep val =
  postfixComma <|> infixComma where
    commaValPair = do _sep <- sep
                      v <- val
                      return v
    infixComma = do first <- val
                    rest <- many commaValPair
                    return (first:rest)
    postfixComma = do vals <- infixComma
                      _sep <- sep
                      return vals

separatedList :: Parser sep -> Parser a -> Parser [a]
separatedList sep val =
  separatedNonemptyList sep val <|> pure []

stmt :: Parser Stmt
stmt = do p <- pattern
          _ <- symbol ":="
          e <- expr
          return $ SetDelayed p e

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
