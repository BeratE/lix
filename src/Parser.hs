-- Functional Parser implementation based on "Programming in Haskell" (2007) G. Hutton.
-- Basic Top-Down (backtracking) recursive descent parser implementation.
-- A parser for lexemes represents a certain token class and the computed value is
-- the respective synthesized token attribute, e.g. the parser nat computing an integer n
-- corresponds to some token <nat, n>.

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f m = Parser $ \s -> parse m s >>= \(v, out) -> Just (f v, out) 

instance Applicative Parser where
  pure  = return
  f <*> m = Parser $ \s -> parse f s >>= \(g, out) -> parse (fmap g m) out

instance Monad Parser where
  return v = Parser $ \s -> Just (v, s)
  m >>= f  = Parser $ \s -> parse m s >>= \(v, out) -> parse (f v) out

-- Choice operator "try parsing p otherwise q"
infixl 9 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \s -> case parse p s of
                            Nothing -> parse q s
                            c@ (Just _) -> c

-- Parse zero or more times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- Parse at least once
many1 :: Parser a -> Parser [a]
many1 p = do {v <- p; vs <- Parser.many p; return (v:vs)}

-- Bottom transition
failure :: Parser a
failure = Parser $ \_ -> Nothing

-- Parse top item of the stream
item :: Parser Char
item = Parser $ \s -> case s of
                        [] -> Nothing
                        (x:xs) -> Just (x, xs)

-- Parse only if predicate holds.                       
sat :: (Char -> Bool) -> Parser Char
sat p = do {x <- item;  if p x then return x else failure}

-- basic lexemes/terminals
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

special :: Parser Char
special = oneOf "_:!#$%%&*+/<=>?@^|-~"

char :: Char -> Parser Char
char x = sat (== x)

-- complex lexemes/terminals
oneOf :: String -> Parser Char
oneOf xs = sat $ \x -> (any . (==)) x xs

untill :: Char -> Parser String
untill x = Parser.many1 $ sat(/= x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do {char x; string xs ; return (x:xs)}

ident :: Parser String
ident = do x <- letter 
           xs <- Parser.many alphanum
           return (x:xs)

sym :: Parser String
sym = do sym <- (ident +++ Parser.many1 special)
         return $ fmap toUpper sym

nat :: Parser Int
nat = do {xs <- many1 digit; return (read xs)}

int :: Parser Int
int = do {x <- char '-'; xs <- nat; return $ negate xs} +++ nat

space :: Parser ()
space = do {Parser.many1 $ (sat isSpace) +++ (char '\n');  return ()}

comment :: Parser ()
comment = do string "--"
             Parser.many $ sat (/= '\n')
             char '\n'
             return () 

blank :: Parser ()
blank = do {Parser.many $ space +++ comment; return ()}

-- token as lexeme that ignores space characters
token :: Parser a -> Parser a
token p = do blank
             x <- p
             blank
             return x
