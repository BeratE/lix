{--
Lambda Calculus implemented as small subset of Lisp SExpr
* Symbols are Variables.
* Special Constants and Symbols denote constant Lambda Terms.
* SExpr (t1 t2 t3 ..) denote (left associative) application.
* Lambda expression is denoted by the constant (Const Lambda),
  in Haskell terms, \x -> y is (Const Lambda, x, y).
  This can be viewed as a sort of deltaRed rule which reduces the
  constant lambda to a lambda expression;
  deltaRed: { ((Const Lambda) x y) => \x -> y
--}
module Eval (eval) where

import LispVal
import Data.List

symconst = ["BOT", "T", "NIL", "QUOTE", "EQ", "IF", "FIX", "LAMBDA"]

-- get free variables of a lambda term
freeVar :: LispVal -> [String]
freeVar t = (fv t) \\ symconst
            where fv (Lit _) = []
                  fv (Symbol s) = [s]
                  fv (List [Symbol "LAMBDA", Symbol s, body]) = delete s $ fv body
                  fv (List xs) = foldr union [] $ fmap fv xs

isClosed :: LispVal -> Bool
isClosed v = null $ (freeVar v)

-- substitute free occurence of symbol x by term t in term r
subst :: String -> LispVal -> LispVal -> LispVal
subst x (List [t]) r = subst x t r
subst x t r@(Lit _) = r
subst x t r@(Symbol s) = if (s == x) then t else r
subst x t r@(List [l@(Symbol "LAMBDA"), p@(Symbol y), b])
    | y == x               = r
    | y `elem` (freeVar t) = subst x t $ rename (y++"`") r
    | otherwise            = List [l, p, subst x t b]
subst x t r@(List rs) = List $ map (subst x t) rs

-- renaming of bound variable y in a lambda expression t (simple alpha reduction)
rename :: String -> LispVal -> LispVal
rename y (List [t]) = rename y t
rename y (List [Symbol "LAMBDA", Symbol x, b])
  = List [Symbol "LAMBDA", Symbol y, subst x (Symbol y) b]
rename y t = t

-- betaRed reduction (lamba abstraction) using leftmost-outermost evaluation
betaRed :: LispVal -> LispVal
betaRed (List (List [Symbol "LAMBDA", Symbol x, b] : t : ts)) =
  let t' = subst x t b in if null ts then t' else List (t' : ts)
betaRed t = rewrite betaRed t

-- deltaRed reduction (constants/functions) using leftmost-outermost evaluation
deltaRed :: LispVal -> LispVal
deltaRed t@(List (Symbol c : ts))
  = case c of
      "IF"    -> funIf ts
      "EQ"    -> funEq ts
      "FIX"   -> funFix ts
      "QUOTE" -> funQuote ts
      otherwise -> Symbol "BOT"
deltaRed t = rewrite deltaRed t

checkDelta :: [LispVal] -> [LispVal]
checkDelta p = if not $ isClosed (List p) then [] else fmap eval p              

-- if exp1 exp2 exp3, lazy evaluation
funIf :: [LispVal] -> LispVal
funIf (t : e1 : e2 : ts)
  = case checkDelta ([t]) of
      [Symbol "NIL"] -> List (eval e2 : ts)
      [_           ] -> List (eval e1 : ts)
      otherwise -> Symbol "BOT"
funIf _ = Symbol "BOT"

-- check if argument one and two are equal
funEq :: [LispVal] -> LispVal
funEq (t1 : t2 : ts)
  = let [t1', t2'] = fmap eval [t1,t2]
    in case [t1', t2'] of
         [Symbol a, Symbol b] -> List (eq a b : ts)
         [Lit    a, Lit    b] -> List (eq a b : ts)
         otherwise -> List (Symbol "NIL" : ts)
    where eq a b = if a == b then Symbol "T" else Symbol "NIL"
funEq _ = Symbol "BOT"

-- fixpoint, fix f => f (fix f)
funFix :: [LispVal] -> LispVal
funFix (t:ts) = List (t : List (Symbol "FIX" : [t]) : ts)
funFix _ = Symbol "BOT"

-- quote is identity on parameters, lazy evaluation
funQuote :: [LispVal] -> LispVal
funQuote [t] = t
funQuote _ = Symbol "BOT"

-- apply reduction to body of lambdas and applications
rewrite :: (LispVal -> LispVal) -> LispVal -> LispVal
rewrite red (List [t]) = red t
rewrite red (List (List [t] : ts)) = red $ List (t : ts)
rewrite red (List [Symbol "LAMBDA", s@(Symbol _), b])
  = let t = red b
    in if t /= (Symbol "BOT")
       then List [Symbol "LAMBDA", s, t]
       else (Symbol "BOT")
rewrite red (List (h:t:ts))
  = let h'  = red h
        ts' = red $ List (t:ts)
    in if h' /= (Symbol "BOT")
       then List (h' : t : ts)
       else case ts' of
              Symbol "BOT" -> Symbol "BOT"
              List lts' -> List (h : lts')
              otherwise -> List (h : [ts'])
rewrite _ _ = Symbol "BOT"

-- Weak Head Normal Order reduction
redWeakHead :: LispVal -> LispVal
redWeakHead (List [t]) = redWeakHead t
redWeakHead t@(List ((Symbol _) : _)) = deltaRed t
redWeakHead t@(List (List [Symbol "LAMBDA", _, _] : _)) = betaRed t
redWeakHead (List (List t : ts))
  = let r = redWeakHead $ List t
    in if r /= Symbol "BOT" then List (r:ts) else Symbol "BOT"
redWeakHead _ = Symbol "BOT"

-- translate a complex term into a simple term
transl :: LispVal -> LispVal
transl (List [t]) = transl t
transl (List [])  = Symbol "NIL"
transl (List [Symbol "LAMBDA", List x', body])
  = case x' of
      []     -> body
      [x]    -> List [Symbol "LAMBDA", x, body]
      (x:xs) -> List [Symbol "LAMBDA", x, transl (List [Symbol "LAMBDA", List xs, body])]
transl (List ts) = List $ map transl ts
transl t = t

-- reduce term until weak head normal form
eval :: LispVal -> LispVal
eval t = evalt $ transl t
         where evalt (List [t]) = evalt t
               evalt t = let r = redWeakHead t
                         in if r == Symbol "BOT"
                            then t
                            else if t == r then Symbol "BOT" else evalt r
