module Interpreter.Comando where

import Control.Monad.State
import Control.Monad.Except

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Expr

import Repr

instance Evaluavel Comando where
  eval (Atribuicao p lv e) = do
    rv <- eval e
    case lv of
      AId nome -> modificarVar nome rv *> return VNada
      -- TODO: Atribuição de lista e de Referência!
      _ -> throwError (FaltaImplementar)

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

  eval (EnquantoCmd p secs) = do
    scp <- novoBloco "ENQUANTO"

    let go [] = pure VNada

        go ((e,cmds):uncs) = do
          v <- eval e
          case v of
            VBool True -> do
              comEscopo id scp (mapM_ eval cmds)
              go secs

            VBool False -> go uncs

            _ -> throwError (TypeError p)

    go secs 
          

  eval (SeCmd p secs) = do
    scp <- novoBloco "SE"

    let go [] = throwError (UnexaustivePatterns p)

        go ((e,cmds):uncs) = do
          v <- eval e
          case v of
            VBool True -> do
              comEscopo id scp (mapM_ eval cmds)
              return VNada

            VBool False -> go uncs

            _ -> throwError (TypeError p)

    go secs 

instance Evaluavel (ProcedimentoR, [Valor]) where
  eval (p, vs) = comEscopo (const ["main"]) nome $ do
    add pars vs -- Inicializa os parâmetros na memória
    mapM eval cs -- Avalia os comandos
    return VNada
    where
      (ProcedimentoR pos i pars cs) = p
      (IdR _ nome) = i

      add :: [Parametro] -> [Valor] -> EvalM ()
      add ((Parametro n _ _) : ps) (v : vs) = addVar n v *> add ps vs
      add [] [] = pure ()
      add _ _ = throwError IncorrectNumberOfParameters
