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

data Stmt
  = SetDelayed Patt Expr
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
  show (DispStmt (SetDelayed p e)) =
    show (toDisp p) ++ " := " ++ show (toDisp e)

class ToDisp a where
  toDisp :: a -> Disp

instance ToDisp Expr where
  toDisp = DispExpr

instance ToDisp Patt where
  toDisp = DispPatt

instance ToDisp Stmt where
  toDisp = DispStmt
