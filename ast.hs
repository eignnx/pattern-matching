module Ast (module Ast) where

import Data.List (intercalate)

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

type Bindings = [(String, Expr)]

data Rule
  = SetDelayed Patt Expr
  | PrimitiveRule Patt (Bindings -> Expr)

instance Show Rule where
  show (SetDelayed p e) = "(SetDelayed $ " ++ show p ++ " $ " ++ show e ++ ")"
  show (PrimitiveRule p f) = "(PrimitiveRule $ " ++ show p ++ " $ {...})"

data Stmt
  = Rule Rule
  deriving (Show)

data Disp
  = DispExpr Expr
  | DispPatt Patt
  | DispStmt Stmt

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
  show (DispStmt (Rule (SetDelayed p e))) =
    show (toDisp p) ++ " := " ++ show (toDisp e)
  show (DispStmt (Rule (PrimitiveRule p f))) =
    show (toDisp p) ++ " := {primitive rule body}"

class ToDisp a where
  toDisp :: a -> Disp

instance ToDisp Expr where
  toDisp = DispExpr

instance ToDisp Patt where
  toDisp = DispPatt

instance ToDisp Stmt where
  toDisp = DispStmt

instance ToDisp a => ToDisp (Maybe a) where
  toDisp (Just x) = toDisp x
  toDisp Nothing = error "expected Just, got nothing in toDisp!"
