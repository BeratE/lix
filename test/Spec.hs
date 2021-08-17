import LispVal

main :: IO ()
main = do val <- readExprFile "test/test.lisp"
          putStrLn $ show val
