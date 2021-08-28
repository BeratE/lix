-- Domain LispVal, Grammar and Parsetree of the little Lisp
module LispVal where

import Parser 
import System.IO
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

-- Syntax Tree in Domain of Lisp Programms
data LispVal = Symbol String | Lit Lit | List [LispVal]
  deriving (Eq)

data Lit = Number Int | String String
  deriving (Eq)

instance Show LispVal where
  show val =
    case val of
      (Lit lit) -> show lit
      (Symbol "FIX") -> "\0966"
      (Symbol "BOT") -> "\0953" 
      (Symbol sym) -> sym
      (List [Symbol "LAMBDA", x, y]) -> "(\0955"++(show x)++"."++(show y)++")"
      (List [Symbol "QUOTE", x]) -> "'" ++ (show x)
      (List ts) -> "(" ++  (if null ts then "" else init (showApp ts)) ++")"
      where showApp [] = []
            showApp (t:ts) = show t ++ " " ++ showApp ts

instance Show Lit where
  show lit =
    case lit of
      Number num -> show num
      String str -> "\"" ++ str ++ "\""


-- Parsers / Grammar for our Lisp with syntax directed translation,
-- i.e. parse input and compute LispVal from (node) attributes
pExpr :: Parser LispVal
pExpr = pSpecial +++ pSymbol +++ pNumber +++ pString +++ pSExpr

-- basic building blocks
pSymbol :: Parser LispVal
pSymbol = Symbol <$> sym

pNumber :: Parser LispVal
pNumber = Lit . Number <$> int

pString :: Parser LispVal
pString = do char '\"'
             s <- untill '\"'
             char '\"'
             return $ Lit $ String s             

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
            return $ List [Symbol "QUOTE", x]

pLambda :: Parser LispVal
pLambda = do char '('
             token $ char '\\'
             vs <- Parser.many1 $ token pSymbol
             x <- case vs of
                    [v]       -> return v
                    _         -> return $ List vs
             token $ char '.'
             y <- token pExpr
             char ')'
             return $ List [Symbol "LAMBDA", x, y]

-- for parsing a sequence of expressions
pList :: Parser LispVal
pList = List <$> many1 (token pExpr)


-- building the parse tree
readExpr :: String -> Maybe LispVal
readExpr s = fst <$> parse (token pExpr) s
  
readExprFile :: String -> IO LispVal
readExprFile file = do contents <- readFile file
                       case parse pList contents of
                         Nothing -> return (Symbol "BOT")
                         Just (v,_) -> return v
