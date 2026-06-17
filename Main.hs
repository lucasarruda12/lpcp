module Main where

import Lexer
import Text.Parsec
import Parser
import Repr
import Interpreter

main :: IO ()
main = do
  let tok = tokenize "IMPRIMA NADA; IMPRIMA 3.12; IMPRIMA 3.54r;"
  let Right ast = parse programaP "" tok
  run (eval ast)
