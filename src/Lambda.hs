module Lambda where

import LispVal
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List

{--
Lambda Calculus implemented as small subset of Lisp SExpr
* Symbols are Variables.
* Special Constants and Symbols denote constant Lambda Terms.
* SExpr (t1 t2 t3 ..) denote (left associative) application.
* Lambda expression is denoted by the constant (Const Lambda),
  in Haskell terms, \x -> y is (Const Lambda, x, y).
  This can be viewed as a sort of delta rule which reduces the
  constant lambda to a lambda expression;
  delta: { ((Const Lambda) x y) => \x -> y
--}

-- translate a complex parse tree into a simple parse tree
transl :: LispVal -> LispVal
transl t@(List [Const Lambda, List x', body]) =
  case x' of
    []     -> body
    [x]    -> List [Const Lambda, x, body]
    (x:xs) -> List [Const Lambda, x, transl (List [Const Lambda, List xs, body])]


-- get free variables of a lambda term
freeVar :: LispVal -> [String]
freeVar (Const _) = []
freeVar (Symbol s) = [s]
freeVar (List [Const Lambda, Symbol s, body]) = delete s $ freeVar body
freeVar (List xs) = foldr union [] $ fmap freeVar xs

isClosed :: LispVal -> Bool
isClosed v = freeVar(v) == []

-- substitute free occurence of x by term r in r
subst :: String -> LispVal -> LispVal -> LispVal
subst x t r@(Const _) = r
subst x t r@(Symbol s) = if (s == x) then t else r
subst x t r@(List [l@(Const Lambda), p@(Symbol y), b])
    | y == x               = r
    | y `elem` (freeVar t) = subst x t $ alpha (y++"`") r
    | otherwise            = List [l, p, subst x t b]
subst x t r@(List rs) = List $ map (subst x t) rs

-- renaming of bound variables in a lambda expression
alpha :: String -> LispVal -> LispVal
alpha y (List [Const Lambda, Symbol x, b])
  = List [Const Lambda, Symbol y, subst x (Symbol y) b]
alpha y r = Const Nil

-- evaluate application of lambda abstraction
beta :: LispVal -> LispVal
beta (List (List [Const Lambda, Symbol x, b] : t2 : ts)) =
  case ts of
    []  -> subst x t2 b
    [_] -> List (subst x t2 b : ts)
beta l = Const Nil

-- evaluate application of constants (i.e. special symbols)
delta :: LispVal -> LispVal
-- if exp1 exp2 exp3, lazy evaluation
delta (List (Const If : ts))
  = case ts of
      [Const (Lit (Bool True)), e2, _ ] -> e2
      [Const (Lit (Bool False)), _, e3] -> e3
      otherwise -> Const Nil
-- quote is identity on parameters, lazy evaluation
delta (List (Const Quote : ts))
  = case ts of
      [] -> Const Nil
      [x] -> x
      otherwise -> List ts
-- fixpoint, fix f => f (fix f)
delta t@(List ((Const Fix) : ts))
  = case ts of
      [] -> Const Nil
      [x] -> List (x : [t])
      otherwise -> List $ (List ts) : [t]

delta c = Const Nil

combOmega :: Maybe LispVal
combOmega = readExpr "((lambda x (x x)) (lambda x (x x)))"

