module Interpreter.Basic where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Data.List (intercalate)
import Interpreter.Erro

import Repr

type EvalM = ExceptT Erro (StateT Ambiente IO)

comPosicao :: Pos -> EvalM a -> EvalM a
comPosicao p m = 
  catchError m (\err -> throwError (Context p err))

class Evaluavel a where
  eval :: a -> EvalM (Valor)

data Ambiente = Am
  { memoria :: Map.Map (Escopo, Id) [Valor]
  , ps :: [ProcedimentoR]
  , escopo :: Escopo
  , cadeia_estatica :: [Escopo]
  --, tipos
  --, fs
  --, contador_de_escopo
  }
  deriving(Show)

ambienteVazio :: Ambiente
ambienteVazio = Am Map.empty [] [] []

data Valor 
  = VInt Int
  | VBool Bool
  | VString String
  | VReal Double
  | VFloat Float
  | VList [Valor]
  | VTuple [Valor]
  | VDict [(Valor, Valor)]
  | VEnum String [Valor]
  | VConstrutor String -- variante do enum com tipo
  | VNada

instance Show Valor where
  show (VInt x) = show x
  show (VBool b) = show b
  show (VString s) = show s
  show (VReal s) = show s
  show (VFloat s) = show s
  show (VList xs) = "[" ++ intercalate "," (map show xs) ++ "]"
  show (VTuple xs) = "(" ++ intercalate "," (map show xs) ++ ")"
  show (VDict pairs) = "{" ++ intercalate ", " (map showPair pairs) ++ "}"
    where showPair (k,v) = show k ++ ": " ++ show v
  show (VEnum nome []) = nome
  show (VEnum nome vs) = nome ++ "(" ++ intercalate "," (map show vs) ++ ")"
  show (VConstrutor nome) = "<construtor " ++ nome ++ ">"
  show VNada = "Nada"

type Escopo = String

addVar :: Id -> Valor -> EvalM ()
addVar nome v = do
  -- TODO: Checar se já existe?
  scp <- gets escopo
  mem <- gets memoria
  modify $ \am 
    -> am { memoria = Map.insertWith (++) (scp, nome) [v] (memoria am) }

getVar :: Id -> EvalM (Valor)
getVar nome = do
  scp <- gets escopo
  mem <- gets memoria
  ces <- gets cadeia_estatica
  getVar' (scp:ces) mem
  where
    getVar' []     mem = throwError $ UndefinedVariable (getPos nome)
    getVar' (e:es) mem = case Map.lookup (e, nome) mem of
      Just (v:_) -> return v
      _ -> getVar' es mem

popEscopo :: Escopo -> EvalM ()
popEscopo scp = do
  mem <- gets memoria
  modify $ \am
    -> am { memoria = Map.mapWithKey (f scp) mem }
  where
    f :: Escopo -> (Escopo, Id) -> [Valor] -> [Valor]
    f scp (scp', _) = if scp == scp' then drop 1 else id

getProc :: Id -> EvalM (ProcedimentoR)
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
