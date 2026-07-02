module Main where

import Lexer
import Text.Parsec
import Parser
import Interpreter
import Analyser
import Control.Monad
import System.Environment (getArgs)
import System.Exit (exitFailure)

rodar :: String -> Bool -> Bool -> IO()
rodar arquivo seguro debug = do
  prog <- readFile arquivo
  let tok = tokenize prog
  let ast = parse programaP "" tok
  case ast of
    Right programa -> do
      when seguro $ do
        case analiseEstatica programa of
          Just erro -> putStrLn erro >> exitFailure
          Nothing -> pure ()
      run programa debug
    Left err -> print err

ajuda :: IO ()
ajuda = do
    putStrLn "Ajuda:"
    putStrLn "  pppempp [OPÇÕES] <arquivo>"
    putStrLn ""
    putStrLn "opções:"
    putStrLn "  --inseguro    Ignora a análise estática"
    putStrLn "  --debug       Imprime a memória do interpretador"

main :: IO ()
main = do
    args <- getArgs

    let seguro = "--inseguro" `notElem` args
    let debug  = "--debug" `elem` args
        arquivos = filter ((/= "--") . take 2) args

    case arquivos of
      [arquivo] -> rodar arquivo seguro debug
      [] -> do
          putStrLn "Erro: Nenhum arquivo informado"
          ajuda
          exitFailure
      _ -> do
          putStrLn "Erro: Multiplos arquivos informados"
          ajuda
          exitFailure
