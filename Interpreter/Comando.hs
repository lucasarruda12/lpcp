module Interpreter.Comando where

import Control.Monad.State
import Control.Monad.Except

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Expr

import Repr

instance Evaluavel Comando where
  eval (ImprimaCmd _ e) = do
    v <- eval e
    liftIO $ print v
    return v

  -- TODO: Não checa tipos
  eval (Inicializacao p i t e) = comPosicao p $ do
    v <- eval e
    -- TODO: Checagem de tipos?
    addVar i v
    return VNada

  eval (ChamadaCmd p nome args) = comPosicao p $ do
    vs <- mapM eval args
    proc <- getProc nome
    eval (proc, vs)

  eval loop@(EnquantoCmd p ((e, cmds):uncs) = do
    v <- eval e
    case v of
      (VBool True) -> do
        mapM_ eval cmds
        eval loop
      (VBool False) -> do
        eval (EnquantoCmd p uncs)
      _ -> throwError (TypeError p)
  eval (EnquantoCmd p []) = pure VNada

  eval (SeCmd p ((e, cmds):uncs)) = do
    v <- eval e 
    case v of
      (VBool b) -> if b then (mapM_ eval cmds *> pure VNada) else eval (SeCmd p uncs)
      _ -> throwError $ TypeError p
  eval (SeCmd p []) = throwError $ UnexaustivePatterns p

instance Evaluavel (ProcedimentoR, [Valor]) where
  eval (p, vs) = do
    ces <- gets cadeia_estatica
    scp <- gets escopo
    modify $ \am -> 
      am { cadeia_estatica = [ "main" ], escopo = nome }
    add pars vs
    mapM eval cs
    modify $ \am ->
      am { cadeia_estatica = ces, escopo = scp }
    popEscopo nome
    return VNada
    where
      (ProcedimentoR pos i pars cs) = p
      (IdR _ nome) = i

      add :: [Parametro] -> [Valor] -> EvalM ()
      add ((Parametro n _ _) : ps) (v : vs) = addVar n v *> add ps vs
      add [] [] = pure ()
      add _ _ = throwError IncorrectNumberOfParameters
