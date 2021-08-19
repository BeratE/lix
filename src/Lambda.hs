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
  This can be viewed as a sort of deltaRed rule which reduces the
  constant lambda to a lambda expression;
  deltaRed: { ((Const Lambda) x y) => \x -> y
--}

-- translate a complex term into a simple term
transl :: LispVal -> LispVal
transl t@(List [_]) = t
transl t@(List [])  = Const Nil
transl t@(List [Const Lambda, List x', body]) =
  case x' of
    []     -> body
    [x]    -> List [Const Lambda, x, body]
    (x:xs) -> List [Const Lambda, x, transl (List [Const Lambda, List xs, body])]
transl (List ts) = List $ map transl ts
transl t = t

-- get free variables of a lambda term
freeVar :: LispVal -> [String]
freeVar (Const _) = []
freeVar (Symbol s) = [s]
freeVar (List [Const Lambda, Symbol s, body]) = delete s $ freeVar body
freeVar (List xs) = foldr union [] $ fmap freeVar xs

isClosed :: LispVal -> Bool
isClosed v = null $ (freeVar v)

-- substitute free occurence of symbol x by term t in term r
subst :: String -> LispVal -> LispVal -> LispVal
subst x (List [t]) r = subst x t r
subst x t r@(Const _) = r
subst x t r@(Symbol s) = if (s == x) then t else r
subst x t r@(List [l@(Const Lambda), p@(Symbol y), b])
    | y == x               = r
    | y `elem` (freeVar t) = subst x t $ rename (y++"`") r
    | otherwise            = List [l, p, subst x t b]
subst x t r@(List rs) = List $ map (subst x t) rs

-- renaming of bound variable y in a lambda expression t (simple alpha reduction)
rename :: String -> LispVal -> LispVal
rename y (List [t]) = rename y t
rename y (List [Const Lambda, Symbol x, b])
  = List [Const Lambda, Symbol y, subst x (Symbol y) b]
rename y t = t

-- betaRed reduction (lamba abstraction) using leftmost-outermost evaluation
betaRed :: LispVal -> LispVal
betaRed (List (List [Const Lambda, Symbol x, b] : t : ts)) =
  let t' = subst x t b in if null ts then t' else List (t' : ts)
betaRed t = rewrite betaRed t

-- deltaRed reduction (constants/functions) using leftmost-outermost evaluation
deltaRed :: LispVal -> LispVal
deltaRed t@(List (Const c : ts))
  = case c of
      If    -> funIf ts
      Fix   -> funFix ts
      --Quote -> funQuote ts
      otherwise -> Const Nil
deltaRed t = rewrite deltaRed t

checkDelta :: [LispVal] -> [LispVal]
checkDelta p = if not $ isClosed (List p) then [] else fmap reduce p              

-- if exp1 exp2 exp3, lazy evaluation
funIf :: [LispVal] -> LispVal
funIf (e1 : e2 : e3 : ts)
  = case checkDelta (e1:e2:[e3]) of
      [Const Nil, _  , e3'] -> List (e3':ts)
      [_        , e2',   _] -> List (e2':ts)
      otherwise -> Const Nil
funIf _ = Const Nil

-- fixpoint, fix f => f (fix f)
funFix :: [LispVal] -> LispVal
funFix (t:ts)
  = let fs= checkDelta [t]
    in if null fs then Const Nil                       
       else case fs of
              [f] -> List (f : List (Const Fix : [f]) : ts)
              otherwise -> Const Nil
funFix _ = Const Nil
{--      
-- quote is identity on parameters, lazy evaluation
funQuote :: [LispVal] -> LispVal
funQuote [ts]
  = case ts of
      [] -> Const Nil
      [t] -> t
      otherwise -> List ts
funQuote _ = Const Nil
--}
-- apply reduction to body of lambdas and applications
rewrite :: (LispVal -> LispVal) -> LispVal -> LispVal
rewrite red (List [t]) = red t
rewrite red (List (List [t] : ts)) = red $ List (t : ts)
rewrite red (List [Const Lambda, s@(Symbol _), b])
  = let t = red b
    in if t /= (Const Nil)
       then List [Const Lambda, s, t]
       else (Const Nil)
rewrite red (List (h:t:ts))
  = let h'  = red h
        ts' = red $ List (t:ts)
    in if h' /= (Const Nil)
       then List (h' : t : ts)
       else case ts' of
              Const Nil -> (Const Nil)
              List lts' -> List (h : lts')
              otherwise -> List (h : [ts'])
rewrite _ _ = Const Nil

-- Weak Head Normal Order reduction
redWeakHead :: LispVal -> LispVal
redWeakHead (List [t]) = redWeakHead t
redWeakHead (List ((Symbol _) : _)) = Const Nil
redWeakHead (List [Const Lambda, Symbol _]) = Const Nil
redWeakHead t@(List ((Const _) : _)) = deltaRed t
redWeakHead t@(List (List [Const Lambda, _, _] : _)) = betaRed t
redWeakHead (List (List t : ts))
  = let r = redWeakHead $ List t
    in if r /= Const Nil then List (r:ts) else Const Nil
redWeakHead t = Const Nil

-- reduce term until weak head normal form
reduce :: LispVal -> LispVal
reduce t = let r = redWeakHead t
           in if r == Const Nil
              then t
              else if t == r then Const Nil else reduce r
