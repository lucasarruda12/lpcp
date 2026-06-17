module Main where

import Lexer
import Text.Parsec
import Parser
import Repr
import Interpreter

main :: IO ()
main = do
  let tok = tokenize "INICIALIZE a :: Booleano COM VERDADEIRO; IMPRIMA a;"
  let Right ast = parse programaP "" tok
  run (eval ast)
