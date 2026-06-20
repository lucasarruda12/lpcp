module Main where

import Lexer
import Text.Parsec
import Parser
import Repr
import Interpreter
import Analyser

main :: IO ()
main = do
  prog <- readFile "programa.pt"
  let tok = tokenize prog
  case (parse programaP "" tok) of
    Right ast -> case analiseEstatica ast of
      Just err -> putStrLn err
      Nothing -> print "tudo ok!"
    Left error -> 
      print error
