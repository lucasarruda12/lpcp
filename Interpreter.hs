module Interpreter (run, eval) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Expr
import Interpreter.Comando

import Repr

instance Evaluavel Programa where
  eval (Programa ps fs ens es cs) = do
    modify $ \amb -> amb
      { ps = ps
      , fs = fs
      , ens = ens
      , es = es
      , escopo = "main" } 
    mapM_ eval cs
    return VNada

run :: EvalM Valor -> IO ()
run m = do
  (result, ambiente) <- runStateT (runExceptT m) ambienteVazio
  case result of
    Left err -> print err
    Right _ -> pure ()
  print ambiente
