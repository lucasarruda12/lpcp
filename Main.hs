module Main where

import Lexer
import Text.Parsec
import Parser
import Repr
import Interpreter

main :: IO ()
main = do
  prog <- readFile "programa.pt"
  let tok = tokenize prog
  case (parse programaP "" tok) of
    Right ast -> run (eval ast)
    Left error -> print error
