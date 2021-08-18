-- Domain LispVal, Grammar and Parsetree of the little Lisp

module LispVal where

import Parser 
import System.IO
import Data.Map(Map)
import qualified Data.Map as Map

type LispEnv = Map.Map String LispVal

-- Domain of Lisp Programms
data LispVal
  = Const Const
  | Symbol String
  | List [LispVal]
  deriving (Eq)

newtype Func = Func { apply :: [LispVal] -> LispVal }

data Const
  = Nil | Quote | Car | Cdr | Cons | If | Lambda | Op String | Lit Lit
  deriving (Eq)

data Lit =
    Bool Bool
  | Number Int
  | String String
  deriving (Eq)

instance Show LispVal where
  show val =
    case val of
      (Symbol sym) -> sym
      (List xs)    -> show xs
      (Const lit)  -> show lit

instance Show Const where
  show const =
    case const of
      Nil    -> "'()"
      Quote  -> "'"
      Car    -> "#CAR"
      Cdr    -> "#CDR"
      Cons   -> "#CONS"
      If     -> "#IF"
      Lambda -> "#LAMBDA"
      Op op  -> "#"++op
      Lit l  -> show l

instance Show Lit where
  show lit =
    case lit of
      Bool True  -> "#T"
      Bool False -> "#F"
      Number num -> show num
      String str -> "\"" ++ str ++ "\""


-- Parsers / Grammar for our Lisp with syntax directed translation,
-- i.e. parse input and compute LispVal from (node) attributes
pExpr :: Parser LispVal
pExpr = pQuote +++ pConst +++ pSymbol +++ pSExpr

pConst :: Parser LispVal
pConst = pKeySym +++ pNumber +++ pString

pKeySym :: Parser LispVal
pKeySym = do {s <- oneOf "+-*/"; return $ Const (Op [s])}
      +++ do s <- sym
             case s of
               "QUOTE"   -> return $ Const Quote
               "NIL"     -> return $ Const Nil
               "CAR"     -> return $ Const Car
               "CDR"     -> return $ Const Cdr
               "CONS"    -> return $ Const Cons
               "IF"      -> return $ Const If
               "LAMBDA"  -> return $ Const Lambda
               "#T"      -> return $ Const $ Lit (Bool True)
               "#F"      -> return $ Const $ Lit (Bool False)
               otherwise -> failure

pQuote :: Parser LispVal
pQuote = do char '\''
            x <- pExpr
            return $ List [Const Quote, x]

pNumber :: Parser LispVal
pNumber = do n <- int
             return $ Const $ Lit $ Number n

pString :: Parser LispVal
pString = do char '\"'
             s <- untill '\"'
             char '\"'
             return $ Const $ Lit $ String s

pSymbol :: Parser LispVal
pSymbol = do p <- sym
             return $ Symbol p

pSExpr :: Parser LispVal
pSExpr = do char '('
            xs <- many $ token pExpr
            char ')'
            return $ List xs

pList :: Parser LispVal
pList = do xs <- many1 $ token pExpr
           return $ List xs

-- building the parse tree
readExpr :: String -> Maybe LispVal
readExpr s = parse pExpr s >>= \(v, _) -> (return v)

readExprFile :: String -> IO LispVal
readExprFile file = do contents <- readFile file
                       Just (v, _) <- return $ parse pList contents
                       return v                                            
