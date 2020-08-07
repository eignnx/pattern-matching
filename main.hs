module Main where

import Data.List (lookup)
import Parser (printParse, parsePanic, pattern, expr, stmt, rule)
import Parse (parse)
import Ast
import Match
import Eval

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
  print $ parse expr "Function[x, x][1]"
  printParse expr "asdg]"
  printParse stmt "Plus[x_, y_] := PrimitivePlus[x, y]"
  putStrLn "DONE."
  let base = [parsePanic rule "Second[x_, y_] := y"]
  --let base = [SetDelayed (ConsP (SymP "Second") [VarP "x", VarP "y"])
                         --(Sym "y")]
  --let res = eval base (Cons (Sym "Second") [Sym "qwerty", Cons (Sym "blah") [Num 1, Num 2]])
  let res = eval base $ parsePanic expr "Second[asdf[1,2], qwerty[blah, foo[]]]"
  print $ toDisp res
