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
  eval (Programa ps es cs) = do
    modify $ \amb ->
      amb { ps = ps, escopo = "main" } 
    mapM_ evalEnum es
    mapM_ eval cs
    return VNada

evalEnum :: EnumDecl -> EvalM ()
evalEnum (EnumDecl _ nome variantes) =
  mapM_ (registrarVariante) variantes
  where
    registrarVariante (VarianteEnum id Nothing) =
      addVar id (VEnum (getId id) [])
    registrarVariante (VarianteEnum id (Just _)) = do
      addVar id (VConstrutor (getId id))
      return ()

getId :: Id -> String
getId (IdR _ s) = s

run :: EvalM (Valor) -> IO ()
run m = do
  (result, ambiente) <- runStateT (runExceptT m) ambienteVazio
  case result of
    Left err -> print err
    Right _ -> pure ()
  -- print ambiente
