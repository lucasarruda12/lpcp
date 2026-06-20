module Interpreter.Basic where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Interpreter.Erro

import Repr

type EvalM = ExceptT Erro (StateT Ambiente IO)

comPosicao :: Pos -> EvalM a -> EvalM a
comPosicao p m = 
  catchError m (throwError . Context p)

class Evaluavel a where
  eval :: a -> EvalM Valor

data Ambiente = Am
  { memoria :: Map.Map (Escopo, Id) [Valor]
  , ps :: [ProcedimentoR]
  , escopo :: Escopo
  , cadeia_estatica :: [Escopo]
  , contagem_de_blocos :: Int
  --, tipos
  --, fs
  --, contador_de_escopo
  }
  deriving(Show)

ambienteVazio :: Ambiente
ambienteVazio = Am Map.empty [] [] [] 0

data Valor 
  = VInt Int
  | VBool Bool
  | VString String
  | VReal Double
  | VFloat Float
  | VNada

instance Show Valor where
  show (VInt x) = show x
  show (VBool b) = show b
  show (VString s) = s
  show (VReal s) = show s
  show (VFloat s) = show s
  show VNada = "Nada"

type Escopo = String

novoBloco :: String -> EvalM Escopo
novoBloco s = do
  scp <- gets ((s ++) . show . contagem_de_blocos)
  modify $ \am
    -> am { contagem_de_blocos = contagem_de_blocos am + 1 }
  return scp

addVar :: Id -> Valor -> EvalM ()
addVar nome v = do
  -- TODO: Checar se já existe?
  scp <- gets escopo
  mem <- gets memoria
  modify $ \am 
    -> am { memoria = Map.insertWith (++) (scp, nome) [v] (memoria am) }

getVar :: Id -> EvalM Valor
getVar nome = do
  scp <- gets escopo
  mem <- gets memoria
  ces <- gets cadeia_estatica
  getVar' (scp:ces) mem
  where
    getVar' []     _ = throwError $ UndefinedVariable (getPos nome)
    getVar' (e:es) mem = case Map.lookup (e, nome) mem of
      Just (v:_) -> return v
      _ -> getVar' es mem

modificarVar :: Id -> Valor -> EvalM ()
modificarVar nome valor = do
  mem <- gets memoria
  e <- resolveVar nome
  
  case Map.lookup (e, nome) mem of
    Just (_ : vs) -> 
      modify $ \am -> am {memoria = Map.insert (e, nome) (valor:vs) mem}
    _ -> throwError (UndefinedVariable $ getPos nome)

resolveVar :: Id -> EvalM Escopo
resolveVar nome = do
  scp <- gets escopo
  ces <- gets cadeia_estatica
  mem <- gets memoria
  resolve (scp : ces) mem
  where 
    resolve :: [Escopo] -> Map.Map (Escopo, Id) [Valor] -> EvalM Escopo
    resolve [] _ = throwError (UndefinedVariable $ getPos nome)
    resolve (e:es) mem = case Map.lookup (e, nome) mem of
      Nothing    -> resolve es mem
      Just (v:_) -> return e

comEscopo :: ([Escopo] -> [Escopo]) -> Escopo -> EvalM a -> EvalM a
comEscopo entrar nome acao = do
  ces <- gets cadeia_estatica
  scp <- gets escopo
  modify $ \am -> 
    am { cadeia_estatica = scp : entrar ces, escopo = nome }

  v <- acao

  mem <- gets memoria
  modify $ \am ->
    am { cadeia_estatica = ces
       , escopo = scp
       , memoria = Map.mapWithKey (limpar nome) mem 
       }

  return v
    where
      limpar :: Escopo -> (Escopo, Id) -> [Valor] -> [Valor]
      limpar scp (scp', _)  = if scp == scp' then drop 1 else id

getProc :: Id -> EvalM ProcedimentoR
getProc nome = do
  ps <- gets ps
  case lookup nome ps of
    Just p -> return p
    Nothing -> throwError $ UndefinedVariable (getPos nome)
  where
    lookup :: Id -> [ProcedimentoR] -> Maybe ProcedimentoR
    lookup name (p@(ProcedimentoR _ name' _ _) : ps)
      | name == name' = Just p
      | otherwise = lookup name ps
    loopup _ [] = Nothing
