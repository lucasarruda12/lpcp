module Interpreter (run) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import qualified Data.Map as Map

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Comando

import Repr

evalPrograma :: Programa -> EvalM ()
evalPrograma (Programa ps fs ens es cs) = do
  modify $ \amb -> amb
    { ps = ps
    , fs = fs
    , ens = ens
    , es = es
    , escopo = "main" } 
  evalCmds cs
  pure ()

run :: Programa -> Bool -> IO ()
run p debug = do
  let m = evalPrograma p
  (result, ambiente) <- runStateT (runExceptT m) ambienteVazio
  case result of
    Left err -> print err
    Right _ -> pure ()
  when debug (print ambiente)
