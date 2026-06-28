module Main where

import Lexer
import Text.Parsec
import Parser
import Interpreter
import Analyser
import Control.Monad
import System.Environment (getArgs)
import Data.Foldable

rodar :: String -> Bool -> IO()
rodar s safe = do
  prog <- readFile s
  let tok = tokenize prog
  let ast = parse programaP "" tok
  case ast of
    Right programa -> do
      when safe 
        (for_ (analiseEstatica programa) putStrLn)
      run programa
    Left err -> print err

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    [] -> putStrLn "Forneça o nome de um arquivo para iniciar"
    ["--unsafe"] -> putStrLn "Forneça o nome de um arquivo para iniciar"
    ["--unsafe", a] -> rodar a False
    [a] -> rodar a True
    _ -> putStrLn "Forneça o nome de um arquivo para iniciar"
