module Match (module Match) where

import Data.List (lookup)
import Ast

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

