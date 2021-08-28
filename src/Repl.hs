module Repl (repl) where

import Parser
import LispVal
import Eval
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Trans.Class
import System.Console.Haskeline

repl :: IO ()
repl = do
  putStrLn "Welcome to the LiX REPL! Enjoy yourself.\n\
           \Type :quit to exit the REPL, type :help for help."
  runInputT defaultSettings doRepl

doRepl :: InputT IO ()
doRepl = do
  minp <- getInputLine "LiX > "
  case minp of
    Nothing        -> outputStrLn "Something went wrong."
    Just (':':cmd) -> procCmd $ words cmd
    Just inp       -> do outputStrLn $ procExpr inp
                         doRepl
            
procExpr :: String -> String
procExpr str
  = let expr = readExpr str
    in case expr of              
         Nothing -> "parse error."
         Just x  -> (show x) ++ "\n" ++ (printExpr $ runReader (eval x) Map.empty)

printExpr :: LispVal -> String
printExpr expr = show expr ++ "\n"

procCmd :: [String] -> InputT IO ()
procCmd ["quit"] = outputStrLn "\nFarewell.\n" >> return () 
procCmd ["help"] = outputStrLn helpText >> doRepl
procCmd ["load"] = outputStrLn "Please specify a proper file handle." >> doRepl
procCmd ["load", file]
  = do expr <- lift $ readExprFile file
       outputStrLn $ show $ runReader (eval expr) Map.empty
       doRepl
procCmd _ = doRepl

helpText :: String
helpText
  = "Interactive LiX REPL (Read-Eval-Print-Loop)\n\
    \Evaluate a lisp-style lix expression and output the result.\n\
    \:help     - Display this help text.\n\
    \:quit     - Exit the program.\n\
    \:load     - Load and evaluate a .lix file."
