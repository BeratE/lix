module Eval (transl, eval) where

import LispVal
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader

type LispEnv = Map String LispVal
type EnvCtx = Reader LispEnv

-- Set of constants and special symbols.
symConstSpecial = Set.fromList ["BOT", "NIL", "QUOTE", "DEF", "LAMBDA"]
symConstOpsD = Set.fromList ["+","-","*"]
symConstFunD = Set.fromList ["FIX", "IF", "EQ", "ATOM", "CAR", "CDR", "CONS"]
symConstD = Set.union symConstFunD symConstOpsD
symConst  = Set.union symConstD symConstSpecial

quote :: LispVal -> LispVal
quote t = List [Symbol "QUOTE", t]
unquote :: LispVal -> LispVal
unquote (List [Symbol "QUOTE", t]) = t
unquote t = t

-- Get free variables of a lambda term. Variables bound to env are not free.
freeVar :: LispVal -> EnvCtx (Set String)
freeVar t = do env <- ask
               return $ Set.difference (fv env t) symConst
    where fv :: LispEnv -> LispVal -> Set String
          fv env (Lit _) = Set.empty
          fv env (List [Symbol "QUOTE", _]) = Set.empty
          fv env (List [Symbol "LAMBDA", Symbol s, body]) = Set.delete s $ fv env body
          fv env (List xs) = foldr Set.union Set.empty $ fmap (fv env) xs
          fv env (Symbol s) = if Map.member s env then Set.empty else Set.singleton s

isClosed :: LispVal -> EnvCtx Bool
isClosed t = Set.null <$> freeVar t

-- Substitute free occurences of symbol x by term t in term r.
subst :: String -> LispVal -> LispVal -> EnvCtx LispVal
subst x t r@(Lit _) = return r
subst x t r@(Symbol s) = return $ if (s == x) then t else r
subst x t r@(List [Symbol "QUOTE", _]) = return r
subst x t r@(List [l@(Symbol "LAMBDA"), p@(Symbol y), b])
    | x == y    = return r
    | otherwise = do fv <- freeVar t
                     if (Set.member y fv)
                     then do k <- rename (y++"`") r
                             subst x t k
                     else do a <- subst x t b
                             return $ List [l, p, a]
subst x t r@(List rs) = do env <- ask
                           let a = fmap (subst x t) rs
                               b = fmap runReader a
                               c = fmap ($ env) b
                           return $ List c


-- Rename bound variable y in a lambda expression t (simple alpha reduction).
rename :: String -> LispVal -> EnvCtx LispVal
rename y (List [Symbol "LAMBDA", Symbol x, b])
  = do r <- subst x (Symbol y) b
       return $ List [Symbol "LAMBDA", Symbol y, r]


ret :: LispVal -> [LispVal] -> LispVal
ret t ts = if null ts then t else List (t : ts)

-- Beta reduction (lamba abstraction) using leftmost-outermost evaluation.
applyBeta :: LispVal -> EnvCtx LispVal
applyBeta (List (List [Symbol "LAMBDA", Symbol x, b] : t : ts)) =
  do r <- subst x t b
     return $ ret r ts
applyBeta _ = return $ Symbol "BOT"


-- Delta reduction (constants/functions) using leftmost-outermost evaluation.
applyDelta :: LispVal -> EnvCtx LispVal
applyDelta (List (Symbol c : ts))
  | Set.member c symConstOpsD = deltaArith c ts
  | otherwise = case c of
        "FIX"  -> deltaFix ts
        "IF"   -> deltaIf ts
        "EQ"   -> deltaEq ts
        "ATOM" -> deltaAtom ts
        "CAR"  -> deltaCar ts
        "CDR"  -> deltaCdr ts
        "CONS" -> deltaCons ts
        _      -> return $ Symbol "BOT"

argsEval :: [LispVal] -> Reader LispEnv [LispVal]
argsEval as = do b <- isClosed (List as)
                 if b then do env <- ask
                              let a = fmap eval as
                                  b = fmap runReader a
                                  c = fmap ($ env) b
                              return $ fmap unquote c
                   else return []


deltaArith :: String -> [LispVal] -> EnvCtx LispVal
deltaArith c (a:b:ts)
  = do args <- argsEval [a,b]
       let perf c a b = case c of
             "+" -> a + b
             "-" -> a - b
             "*" -> a * b         
       return $ case args of
         [Lit (Number a'), Lit (Number b')] -> ret (Lit (Number (perf c a' b'))) ts
         otherwise -> Symbol "BOT"
deltaArith _ _= return $ Symbol "BOT"

deltaFix :: [LispVal] -> EnvCtx LispVal
deltaFix ts = return $
  List ((List [Symbol "LAMBDA", Symbol "F",
                List [Symbol "F", List [Symbol "FIX", Symbol "F"]]]) : ts)

deltaIf :: [LispVal] -> EnvCtx LispVal
deltaIf (t : ts)
  = do args <- argsEval [t]
       let f val = List [Symbol "LAMBDA", Symbol "X",
                         (List [Symbol "LAMBDA", Symbol "Y", val])]
       return $ case args of
         [Symbol "BOT"] -> Symbol "BOT"
         [Symbol "NIL"] -> ret (f (Symbol "Y")) ts
         [_]            -> ret (f (Symbol "X")) ts
         otherwise      -> Symbol "BOT"
deltaIf _ = return $ Symbol "BOT"

deltaEq :: [LispVal] -> EnvCtx LispVal
deltaEq (t1 : t2 : ts)
  = do argsEval <- argsEval [t1,t2]
       let eq a b = if a == b then (Lit $ Number 1)
                    else Symbol "NIL"
       return $ case argsEval of
         [Symbol "BOT", _] -> Symbol "BOT"
         [_, Symbol "BOT"] -> Symbol "BOT"
         [a, b] -> ret (eq a b) ts
         otherwise -> Symbol "BOT"
deltaEq _ = return $ Symbol "BOT"
           
deltaAtom :: [LispVal] -> EnvCtx LispVal
deltaAtom (t:ts)
  = do args <- argsEval [t]
       return $ case args of 
         [Symbol "BOT"] -> Symbol "BOT"
         [Symbol _]     -> ret (Lit $ Number 1) ts
         [Lit    _]     -> ret (Lit $ Number 1) ts
         [List []]      -> ret (Lit $ Number 1) ts
         [_]            -> ret (Symbol "NIL") ts
         otherwise      -> Symbol "BOT"
deltaAtom _ = return $ Symbol "BOT"

deltaCons :: [LispVal] -> EnvCtx LispVal
deltaCons (t1 : t2 : ts)
  = do args <- argsEval [t1, t2]
       return $ case args of
         [Symbol "BOT", _] -> Symbol "BOT"
         [List _,       _] -> Symbol "BOT"
         [a,      List bs] -> ret (quote $ List (a:bs)) ts
         [a, Symbol "NIL"] -> ret (quote $ List [a]) ts
         otherwise         -> Symbol "BOT"
deltaCons _ = return $ Symbol "BOT"

deltaCar :: [LispVal] -> EnvCtx LispVal
deltaCar (t : ts)
  = do args <- argsEval [t]
       return $ case args of 
         [List (a:as)] -> ret (quote a) ts
         otherwise     -> Symbol "BOT"
deltaCar _ = return $ Symbol "BOT"

deltaCdr :: [LispVal] -> EnvCtx LispVal
deltaCdr (t:ts)
  = do args <- argsEval [t]
       return $ case args of 
         [List (a:as)] -> ret (quote $ List as) ts
         otherwise     -> Symbol "BOT"
deltaCdr _ = return $ Symbol "BOT"


-- Translate a complex term into a simple term
transl :: LispVal -> LispVal
transl (List [t]) = transl t

transl t@(List [Symbol "QUOTE", _]) = t
transl (List [Symbol "LET", f@(Symbol _), val, expr])
  = let b = List [Symbol "FIX", (List [Symbol "LAMBDA", f, (transl val)])]
        a = List [Symbol "LAMBDA", f, (transl expr)]
    in List [a, b]
       
transl (List [Symbol "COND", List ((List [p,e]) : cs)])
  | null cs   =transl$ List [Symbol "IF", p, e, Symbol "NIL"]
  | otherwise =transl$ List [Symbol "IF", p, e, transl (List [Symbol "COND", List (cs)])]
  
transl (List [Symbol "LAMBDA", List x', body])
  = case x' of
      []     -> body
      [x]    -> List [Symbol "LAMBDA", x, body]
      (x:xs) -> List [Symbol "LAMBDA", x, transl (List [Symbol "LAMBDA", List xs, body])]
      
transl (List ts) = List $ map transl ts
transl t = t


-- Reduce term until closed normal form.
eval :: LispVal -> EnvCtx LispVal
eval t = let tr = transl t
         in do r <- reduce tr
               return r

reduce :: LispVal -> EnvCtx LispVal
reduce t@(Lit _) = return t
reduce t@(List [Symbol "QUOTE", _]) = return t
reduce t@(List [Symbol "LAMBDA", Symbol _, _]) = return t

reduce t@(Symbol s)
  = if s `elem` symConst
    then return t
    else do asks (Map.findWithDefault (Symbol "BOT") s)
  
reduce (List [t]) = reduce t

reduce t@(List (Symbol c: _)) = do {d <- applyDelta t; reduce d}

reduce t@(List (List [Symbol "LAMBDA", _, _] : _)) = do {b <- applyBeta t; reduce b}

reduce (List ((List [Symbol "DEF", Symbol f, expr]) : ts))
  = local (Map.insert f expr) $ reduce (List ts)

reduce (List (t : ts)) =
  do r <- reduce t
     if r == Symbol "BOT" || r == t
       then return $ Symbol "BOT"
       else do k <- reduce (List (r : ts))
               return k
               

reduce _ = return $ Symbol "BOT"
