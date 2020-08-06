module Main where

import Data.List (lookup, intercalate)
import Parse (Parser, integer, identifier, char, parse, symbol)
import Control.Applicative

data Expr
  = Cons Expr [Expr]
  | Sym String
  | Num Int
  deriving (Show, Eq)

data Patt
  = ConsP Patt [Patt]
  | SymP String
  | NumP Int
  | VarP String
  deriving (Show)

data Disp
  = DispExpr Expr
  | DispPatt Patt

instance Show Disp where
  show (DispExpr (Num n)) = show n
  show (DispExpr (Sym s)) = s
  show (DispExpr (Cons hd tl)) =
    show (toDisp hd) ++ "[" ++ intercalate ", " (map (show . toDisp) tl) ++ "]"
  show (DispPatt (NumP n)) = show n
  show (DispPatt (SymP s)) = s
  show (DispPatt (VarP v)) = v ++ "_"
  show (DispPatt (ConsP hd tl)) =
    show (toDisp hd) ++ "[" ++ intercalate ", " (map (show . toDisp) tl) ++ "]"

class ToDisp a where
  toDisp :: a -> Disp

instance ToDisp Expr where
  toDisp = DispExpr

instance ToDisp Patt where
  toDisp = DispPatt


type Bindings = [(String, Expr)]

match :: Bindings
      -> Patt
      -> Expr
      -> Maybe Bindings
match bs (SymP a) (Sym b) | a == b = Just bs
match bs (NumP a) (Num b) | a == b = Just bs
match bs (VarP x) v = case lookup x bs of
  Nothing -> Just $ (x, v):bs
  Just prevV -> if prevV == v then Just bs else Nothing
match bs (ConsP hdP tlP) (Cons hdV tlV) = do
  bs' <- match bs hdP hdV
  matchList bs' tlP tlV where
    matchList :: Bindings -> [Patt] -> [Expr] -> Maybe Bindings
    matchList bs [] [] = Just bs
    matchList bs (p:ps) (v:vs) = do
      bs' <- match bs p v
      matchList bs' ps vs
match _ _ _ = Nothing

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


main :: IO ()
main = do
  let p = ConsP (SymP "Plus") [VarP "x", VarP "y"]
  let v = Cons (Sym "Plus") [Num 123, Sym "a"]
  print p
  print v
  print $ match [] p v
  putStrLn "---------------------------"
  printParse pattern "Plus[1, x_, 3,]"
  printParse pattern "Plus[1, x_, 3]"
  printParse pattern "Plus[3]"
  printParse pattern "Plus[3,]"
  printParse pattern "Plus[]"
  putStrLn "---------------------------"
  printParse expr "Plus[1, x]"
  printParse expr "Function[x, x][1]"
  printParse expr "asdg]"
  putStrLn "DONE."
