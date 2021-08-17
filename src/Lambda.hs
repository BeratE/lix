module Lambda where

import LispVal
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List

{--
Lambda Calculus implemented as small subset of Lisp SExpr
* Symbols are Variables
* Literals and some special symbols are Constants
* Special SExpr of the form ("LAMBDA" x body) denotes a lambda expression
* Other SExpr are evaluated as applications (left associative),
  e.g. (t1 t2 t3 ..) = (..((t1 t2) t3) ..)
--}

-- evaluate using weak-head normal order reduction
eval :: LispVal -> LispVal
eval l@(Lit _ ) = l          

-- get free variables of a lambda term
freeVar :: LispVal -> [String]
freeVar (Lit _) = []
freeVar (Symbol s) = [s]
freeVar (List [(Symbol "LAMBDA"), (Symbol s), body]) = delete s $ freeVar body
freeVar (List xs) = foldr union [] $ fmap freeVar xs

isClosed :: LispVal -> Bool
isClosed v = freeVar(v) == []

-- substitute free occurence of x by term r in r
subst :: String -> LispVal -> LispVal -> LispVal
subst x t r@(Lit _) = r
subst x t r@(Symbol s) = if (s == x) then t else r
subst x t r@(List [l@(Symbol "LAMBDA"), p@(Symbol y), b])
    | y == x               = r
    | y `elem` (freeVar t) = subst x t $ alpha (y++"`") r
    | otherwise            = List [l, p, subst x t b]
subst x t r@(List rs) = List $ map (subst x t) rs

-- renaming of bound variables in a lambda expression
alpha :: String -> LispVal -> LispVal
alpha y r@(List [l@(Symbol "LAMBDA"), p@(Symbol x), b])
  = List [l, (Symbol y), subst x (Symbol y) b]
alpha y r = r

-- evaluate application of lambda abstraction
beta :: LispVal -> LispVal
beta l@(List (t1 : t2 : ts)) =
  case t1 of
    r@(List [l@(Symbol "LAMBDA"), p@(Symbol x), b])
      -> subst x t2 b
    otherwise -> Lit LNil
beta l = Lit LNil

-- evaluate application of constants (i.e. special symbols)
delta :: LispVal -> LispVal
delta (List [(Symbol s), (Lit (LNumber x)), (Lit (LNumber y))])
  = case s of
      "+" -> Lit $ LNumber (x + y)
      "-" -> Lit $ LNumber (x - y)
      otherwise -> Lit LNil
delta (List [(Symbol s), expr1, expr2, expr3])
  = case s of
      "IF" -> if (expr1 == (Lit $ LBool True)) then expr2 else expr3
      otherwise -> Lit LNil
delta c = Lit LNil


combOmega :: Maybe LispVal
combOmega = readExpr "((lambda x (x x)) (lambda x (x x)))"

