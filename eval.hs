module Eval (module Eval) where

import Ast
import Match

matchRuleBase :: [Rule] -> Expr -> Maybe (Bindings, Expr)
matchRuleBase rules expr =
  let matchRule (SetDelayed p e) = case match [] p expr of
                                        Just bs -> Just (bs, e)
                                        Nothing -> Nothing 
  in case mapM matchRule rules of
    Just ((bs, e):_) -> Just (bs, e) -- For now just return first one
    Nothing -> Nothing

eval :: [Rule] -> Expr -> Maybe Expr
eval rules (Num n) = Just (Num n)
eval rules (Sym s) = case matchRuleBase rules (Sym s) of
  Nothing -> Just (Sym s)
  Just (_bs, e) -> Just e -- There are no possible `bs`s for a `Sym`.
eval rules (Cons hd args) = case matchRuleBase rules (Cons hd args) of
  Just (bs, e) -> Just (evalWithBindings bs e)
  Nothing -> case matchRuleBase rules hd of
    Just (bs, e) -> let hd' = evalWithBindings bs e in
      case eval rules (Cons hd' args) of
        Nothing -> let args' = evalList rules args in
          eval rules (Cons hd' args')
        x -> x
    Nothing -> let args' = evalList rules args in
      eval rules (Cons hd args')

evalList :: [Rule] -> [Expr] -> [Expr]
evalList rules [] = []
evalList rules (e:es) =
  case eval rules e of
    Just e' -> e':(evalList rules es)
    Nothing -> e:(evalList rules es)

evalWithBindings :: Bindings -> Expr -> Expr
evalWithBindings bs (Num n) = Num n
evalWithBindings bs (Sym s) =
  case lookup s bs of
       Just e -> e
       Nothing -> Sym s -- If no binding can be found, return the symbol itself.
evalWithBindings bs (Cons hd args) =
  let hd' = evalWithBindings bs hd
  in let args' = map (evalWithBindings bs) args
  in Cons hd' args'

