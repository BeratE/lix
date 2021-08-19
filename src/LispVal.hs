-- Domain LispVal, Grammar and Parsetree of the little Lisp

module LispVal where

import Parser 
import System.IO
import Data.Map(Map)
import qualified Data.Map as Map

type LispEnv = Map.Map String LispVal

-- Syntax Tree in Domain of Lisp Programms
data LispVal
  = Const Const
  | Symbol String
  | List [LispVal]
  deriving (Eq)

data Const
  = Fix | Nil | Quote | Car | Cdr | Cons | If | Lambda | Op String | Lit Lit
  deriving (Eq)

data Lit
  = Number Int
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
      Fix    -> "#FIX"
      Nil    -> "'()"
      Quote  -> "'"
      Car    -> "#CAR"
      Cdr    -> "#CDR"
      Cons   -> "#CONS"
      If     -> "#IF"
      Lambda -> "#\\"
      Op op  -> "#"++op
      Lit l  -> show l

instance Show Lit where
  show lit =
    case lit of
      Number num -> show num
      String str -> "\"" ++ str ++ "\""


-- Parsers / Grammar for our Lisp with syntax directed translation,
-- i.e. parse input and compute LispVal from (node) attributes
pExpr :: Parser LispVal
pExpr = pSpecial +++ pConst +++ pSymbol +++ pSExpr

-- basic building blocks
pSymbol :: Parser LispVal
pSymbol = do p <- sym
             return $ Symbol p

pSExpr :: Parser LispVal
pSExpr = do char '('
            xs <- many $ token pExpr
            char ')'
            return $ List xs

-- enforce special syntactical rules, convert to normal sexpr
pSpecial :: Parser LispVal 
pSpecial = pQuote +++ pLambda

pQuote :: Parser LispVal
pQuote = do char '\''
            x <- pExpr
            return $ List [Const Quote, x]

pLambda :: Parser LispVal
pLambda = do char '('
             token $ char '\\'
             vs <- Parser.many1 $ token pSymbol
             x <- case vs of
                    [v]       -> return v
                    otherwise -> return $ List vs
             token $ char '.'
             y <- token pExpr
             char ')'
             return $ List [Const Lambda, x, y]

-- constant terms, i.e. reserved key words and literals
pConst :: Parser LispVal
pConst = pKeyword +++ pNumber +++ pString

pKeyword :: Parser LispVal
pKeyword = do {s <- oneOf "+-*/"; return $ Const (Op [s])}
       +++ do s <- sym
              case s of
                "FIX"     -> return $ Const Fix
                "QUOTE"   -> return $ Const Quote
                "NIL"     -> return $ Const Nil
                "CAR"     -> return $ Const Car
                "CDR"     -> return $ Const Cdr
                "CONS"    -> return $ Const Cons
                "IF"      -> return $ Const If
                "LAMBDA"  -> return $ Const Lambda
                otherwise -> failure             
             
-- literals
pNumber :: Parser LispVal
pNumber = do n <- int
             return $ Const $ Lit $ Number n

pString :: Parser LispVal
pString = do char '\"'
             s <- untill '\"'
             char '\"'
             return $ Const $ Lit $ String s


-- for parsing a sequence of expressions
pList :: Parser LispVal
pList = do xs <- many1 $ token pExpr
           return $ List xs


-- building the parse tree
readExpr :: String -> Maybe LispVal
readExpr s = parse (token pExpr) s >>= \(v, _) -> (return v)

readExprFile :: String -> IO LispVal
readExprFile file = do contents <- readFile file
                       Just (v, _) <- return $ parse pList contents
                       return v                                            
