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
  let Right ast = parse programaP "" tok
  run (eval ast)
