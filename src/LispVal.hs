-- Domain LispVal, Grammar and Parsetree of the little Lisp

module LispVal where

import Parser 
import System.IO
import Data.Map(Map)
import qualified Data.Map as Map

type LispEnv = Map.Map String LispVal

-- Domain of Lisp Programms
data LispVal
  = Lit Lit
  | Symbol String
  | List [LispVal]
  | Lambda Func LispEnv

newtype Func = Func { apply :: [LispVal] -> LispVal }

data Lit
  = LNil
  | LBool Bool
  | LNumber Int
  | LString String
  deriving (Eq)

instance Eq LispVal where
  (==) (Lit x) (Lit y)             = x == y
  (==) (Symbol s1) (Symbol s2)     = s1 == s2
  (==) (List (x:xs)) (List (y:ys)) = x == y && (List xs) == (List ys)
  (==) _ _                         = False

instance Show LispVal where
  show val =
    case val of
      (Symbol sym) -> sym
      (List xs)    -> show xs
      (Lit lit)    -> show lit
      (Lambda _ _) -> "lambda function"

instance Show Lit where
  show lit =
    case lit of
      (LBool True)  -> "#T"
      (LBool False) -> "#F"
      (LNumber num) -> show num
      (LString str) -> "\"" ++ str ++ "\""
      LNil          -> "'()"


-- Parsers / Grammar for our Lisp with syntax directed translation,
-- i.e. parse input and compute LispVal from (node) attributes
pExpr :: Parser LispVal
pExpr = pLit +++ pQuote +++ pSymbol +++ pSExpr

pLit :: Parser LispVal
pLit = pReserved +++ pNumber +++ pString

pReserved :: Parser LispVal
pReserved = do string "NIL"
               return $ Lit LNil
             +++ (do string "#T"
                     return $ Lit (LBool True))
             +++ (do string "#F"
                     return $ Lit (LBool False))

pNumber :: Parser LispVal
pNumber = do n <- int
             return $ Lit $ LNumber n

pSymbol :: Parser LispVal
pSymbol = do p <- sym
             return $ Symbol p

pString :: Parser LispVal
pString = do char '\"'
             s <- untill '\"'
             char '\"'
             return $ Lit $ LString s

pQuote :: Parser LispVal
pQuote = do char '\''
            x <- pExpr
            return $ List [Symbol "quote", x]

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
