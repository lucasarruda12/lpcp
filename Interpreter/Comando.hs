module Interpreter.Comando where

import Control.Monad.State
import Control.Monad.Except
import Data.Functor

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Expr

import Repr

instance Evaluavel Comando where
  eval (Atribuicao p lv e) = do
    rv <- eval e
    case lv of
      AId nome -> modificarVar nome rv $> VNada
      -- TODO: Atribuição de lista e de Referência!
      _ -> throwError FaltaImplementar

  eval (ImprimaCmd _ e) = do
    v <- eval e
    liftIO $ print v
    return v

  eval (Inicializacao p i t e) = comPosicao p $ do
    v <- eval e
    addVar i v
    return VNada

  eval (ChamadaCmd p nome args) = comPosicao p $ do
    proc <- getProc nome
    eval (proc, args)

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

instance Evaluavel (ProcedimentoR, [Expr]) where
  eval (p, es) = comEscopo (const ["main"]) nome $ do
    add pars es -- Inicializa os parâmetros na memória
    mapM_ eval cs -- Avalia os comandos
    return VNada
    where
      (ProcedimentoR pos i pars cs) = p
      (IdR _ nome) = i

      add :: [Parametro] -> [Expr] -> EvalM ()
      add ((Parametro n _ porref) : ps) (e : es) 
        | porref = case e of
          EVar id -> do
            scp <- resolveVar id
            addVar n (VRef (scp, id))
          _ -> throwError FaltaImplementar
        | otherwise = (eval e >>= addVar n) *> add ps es

      add [] [] = pure ()
      add _ _ = throwError IncorrectNumberOfParameters
