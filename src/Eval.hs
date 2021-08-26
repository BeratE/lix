module Eval where

import LispVal
import Data.List

-- collection of constants and special symbols
symConstOps = ["+","-","*"]
symConstFun = ["FIX", "IF", "EQ", "ATOM", "CAR", "CDR", "CONS"]
symConst    = symConstFun ++ symConstOps
symSpecial  = ["BOT", "NIL", "QUOTE", "LAMBDA"]
constSym    = symSpecial ++ symConst

-- helper functions
ret t ts = if null ts then t else List (t : ts)
isClosed v = null $ (freeVar v)
getArgs p = fmap unquote $ if isClosed (List p) then fmap eval p else []
unquote (List [Symbol "QUOTE", t]) = t
unquote t = t

-- get free variables of a lambda term
freeVar :: LispVal -> [String]
freeVar t = (fv t) \\ constSym
            where fv (Lit _) = []
                  fv (Symbol s) = [s]
                  fv (List [Symbol "QUOTE", _]) = []
                  fv (List [Symbol "LAMBDA", Symbol s, body]) = delete s $ fv body
                  fv (List xs) = foldr union [] $ fmap fv xs

-- substitute free occurence of symbol x by term t in term r
subst :: String -> LispVal -> LispVal -> LispVal
subst x (List [t]) r = subst x t r
subst x t r@(Lit _) = r
subst x t r@(Symbol s) = if (s == x) then t else r
subst x t r@(List [Symbol "QUOTE", _]) = r
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
applyBeta :: LispVal -> LispVal
applyBeta (List (List [Symbol "LAMBDA", Symbol x, b] : t : ts)) =
  let t' = subst x t b in ret t' ts
applyBeta _ = Symbol "BOT"

-- deltaRed reduction (constants/functions) using leftmost-outermost evaluation
applyDelta :: LispVal -> LispVal
applyDelta (List (Symbol c : ts))
  | c `elem` symConstOps = deltaArith c ts
  | c == "FIX"           = deltaFix ts 
  | c == "IF"            = deltaIf ts  
  | c == "EQ"            = deltaEq ts  
  | c == "ATOM"          = deltaAtom ts
  | c == "CAR"           = deltaCar ts
  | c == "CDR"           = deltaCdr ts
  | c == "CONS"          = deltaCons ts
  | otherwise            =  Symbol "BOT"
applyDelta _ = Symbol "BOT"

-- perform arithmetic operations on two values
deltaArith :: String -> [LispVal] -> LispVal
deltaArith c (a:b:ts)
  = let args = getArgs [a,b]       
        perf c a b = case c of
          "+" -> a + b
          "-" -> a - b
          "*" -> a * b         
    in case args of
         [Lit (Number a'), Lit (Number b')] -> ret (Lit (Number (perf c a' b'))) ts
         otherwise -> Symbol "BOT"

-- fixpoint, fix f => f (fix f)
deltaFix :: [LispVal] -> LispVal
deltaFix ts = List ((List [Symbol "LAMBDA", Symbol "F",
                           List [Symbol "F", List [Symbol "FIX", Symbol "F"]]]) : ts)

-- if t, returns corresponding lambda expression for t equal to nil or non-nil
deltaIf :: [LispVal] -> LispVal
deltaIf (t : ts)
  = let f val = List [Symbol "LAMBDA", Symbol "X",
                      (List [Symbol "LAMBDA", Symbol "Y", val])]
    in case getArgs [t] of
         [Symbol "BOT"] -> Symbol "BOT"
         [Symbol "NIL"] -> ret (f (Symbol "Y")) ts
         [_]            -> ret (f (Symbol "X")) ts
         otherwise      -> Symbol "BOT"
deltaIf _ = Symbol "BOT"

-- check if argument one and two are equal
deltaEq :: [LispVal] -> LispVal
deltaEq (t1 : t2 : ts)
  = let args = getArgs [t1,t2]
        eq a b = if a == b then (Lit $ Number 1)
          else Symbol "NIL"
    in case args of
         [Symbol "BOT", _] -> Symbol "BOT"
         [_, Symbol "BOT"] -> Symbol "BOT"
         [a, b] -> ret (eq a b) ts
         otherwise -> Symbol "BOT"
deltaEq _ = Symbol "BOT"
           
-- check if argument is a lit or symbol
deltaAtom :: [LispVal] -> LispVal
deltaAtom (t:ts)
  = let t' = getArgs [t]
    in case t' of
         [a@(Symbol _)] -> ret (Lit $ Number 1) ts
         [a@(Lit    _)] -> ret (Lit $ Number 1) ts
         [_]            -> Symbol "NIL"
         otherwise      -> Symbol "BOT"
deltaAtom _ = Symbol "BOT"

deltaCons :: [LispVal] -> LispVal
deltaCons (t1 : t2 : ts)
  = let a = deltaAtom [t1]
        in if a == Symbol "NIL" || a == Symbol "BOT"
           then Symbol "BOT"
           else case getArgs [t2] of
                  [List bs] -> ret (List (a:bs)) ts
                  otherwise  -> Symbol "BOT"
deltaCons _ = Symbol "BOT"

deltaCar :: [LispVal] -> LispVal
deltaCar (t : ts)
  = case getArgs [t] of
      [List (a:as)] -> ret (List [Symbol "QUOTE", a]) ts
      otherwise     -> Symbol "BOT"
deltaCar _ = Symbol "BOT"

deltaCdr :: [LispVal] -> LispVal
deltaCdr (t:ts)
  = case getArgs [t] of
      [List (a:as)] -> ret (List [Symbol "QUOTE", List as]) ts
      otherwise     -> Symbol "BOT"
deltaCdr _ = Symbol "BOT"


-- Weak Head Normal Order reduction, one step using leftmost-outermost 
reduce :: LispVal -> LispVal
reduce (List [t]) = reduce t
reduce t@(Lit _) = t
reduce t@(Symbol _) = t
reduce t@(List (Lit _ : _)) = t
reduce t@(List [Symbol "LAMBDA", Symbol _, _]) = t
reduce t@(List (Symbol c: _)) = if c `elem` symConst then applyDelta t else t
reduce t@(List (List [Symbol "LAMBDA", _, _] : _)) = applyBeta t
reduce (List (List t : ts))
  = let r = reduce $ List t
    in if r == Symbol "BOT" then Symbol "BOT" else List (r:ts)                        
reduce _ = Symbol "BOT"


-- translate a complex term into a simple term
transl :: LispVal -> LispVal
transl (List [t]) = transl t
transl (List [])  = Symbol "NIL"

transl (List [Symbol "LET", f@(Symbol _), val, expr])
  = let b = List [Symbol "FIX", (List [Symbol "LAMBDA", f, (transl val)])]
        a = List [Symbol "LAMBDA", f, (transl expr)]
    in List [a, b]
              
transl (List [Symbol "COND", List ((List [p,e]) : cs)])
  | null cs   = transl $ List [Symbol "IF", p, e, Symbol "NIL"]
  | otherwise = transl $ List [Symbol "IF", p, e, transl (List [Symbol "COND",
                                                                List (cs)])]
                
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
        evalt t = let r = reduce t
                  in if r == Symbol "BOT" || r == t
                     then r else evalt r
