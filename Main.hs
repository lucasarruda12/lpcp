module Main where

import Lexer
import Text.Parsec
import Parser
import Repr
import Interpreter
import Analyser

main :: IO ()
main = do
  prog <- readFile "programa2.pt"
  let tok = tokenize prog
  let ast = parse programaP "" tok
  case ast of
    Right programa -> run (eval programa)
    Left err -> print err
  -- case parse programaP "" tok of
  --   Right ast -> case analiseEstatica ast of
  --     Just err -> putStrLn err
  --     Nothing -> print "tudo ok!"
  --   Left error -> 
  --     print error
