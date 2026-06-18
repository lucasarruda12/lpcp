module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Float (float2Double, double2Float)

import Repr

-- O pai estático de qualquer função ou procedimento é sempre o escopo glboal.
-- Minha ideia pra aproveitar isso é: ter um escopo local e um escopo global.
-- As funções só podem acessar a cabeça da lista do escopo_local
-- Os operadores condicionais se, enquanto podem sair procurando por todo o escopo local
-- Todo mundo, em último caso, olha o escopo global.

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
  show (VString s) = show s
  show (VReal s) = show s
  show (VFloat s) = show s
  show VNada = "Nada"

data Erro 
  = TypeError Pos
  | UndefinedVariable Pos
  | AlreadyDefinedVariable Pos
  | IncorrectNumberOfParameters
  | Context Pos Erro
  deriving (Show)

comPosicao :: Pos -> EvalM a -> EvalM a
comPosicao p m = 
  catchError m (\err -> throwError (Context p err))

type EvalM = ExceptT Erro (StateT Ambiente IO)

type Escopo = String

data Ambiente = Am
  { memoria :: Map.Map (Escopo, Id) [Valor]
  , ps :: [ProcedimentoR]
  , escopo :: Escopo
  , cadeia_estatica :: [Escopo]
  --, tipos
  --, fs
  -- ...
  }
  deriving(Show)

ambienteVazio :: Ambiente
ambienteVazio = Am Map.empty [] [] []

addVar :: Id -> Valor -> EvalM ()
addVar nome v = do
  -- TODO: Checar se já existe?
  scp <- gets escopo
  mem <- gets memoria
  case Map.lookup (scp, nome) mem of
    Just _ -> throwError $ AlreadyDefinedVariable (getPos nome)
    Nothing -> do
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

class Evaluavel a where
  eval :: a -> EvalM (Valor)

instance Evaluavel Lit where
  eval (LInt _ x) = pure $ VInt x
  eval (LBool _ b) = pure $ VBool b
  eval (LString _ s) = pure $ VString s
  eval (LNada _) = pure $ VNada
  eval (LReal _ r) = pure $ VReal r
  eval (LFloat _ f) = pure $ VFloat f

evalOpBin :: OpBin -> Valor -> Valor -> Maybe Valor
evalOpBin op (VInt x) (VInt y) = Just $ case op of
  Soma -> VInt (x + y)
  Sub -> VInt (x - y)
  Mul -> VInt (x * y)
  Div -> VInt (x `div` y)
  Exp -> VInt (x ^ y)
  Mod -> VInt (x `mod` y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)

evalOpBin op (VReal x) (VReal y) = Just $ case op of
  Soma -> VReal (x + y)
  Sub -> VReal (x - y)
  Mul -> VReal (x * y)
  Div -> VReal (x / y)
  Exp -> VReal (x ** y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)
  DiferenteOp -> VBool (x /= y)

evalOpBin op (VFloat x) (VFloat y) = Just $ case op of
  Soma -> VFloat (x + y)
  Sub -> VFloat (x - y)
  Mul -> VFloat (x * y)
  Div -> VFloat (x / y)
  Exp -> VFloat (x ** y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)
  DiferenteOp -> VBool (x /= y)

evalOpBin op (VBool b1) (VBool b2) = Just $ case op of
  AndOp -> VBool (b1 && b2)
  OrOp -> VBool (b1 || b2)

evalOpBin _ _ _ = Nothing

evalOpUn :: OpUn -> Valor -> Maybe Valor
evalOpUn Neg (VInt x) = Just $ VInt (-x)
evalOpUn NaoOp (VBool b) = Just $ VBool (not b)
evalOpUn ConvInt v = Just $ case v of
  (VInt x) -> VInt x
  (VReal x) -> VInt (truncate x)
  (VFloat x) -> VInt (truncate x)
  (VBool True) -> VInt 1
  (VBool False) -> VInt 0
  (VString s) -> VInt (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn ConvBool v = Just $ case v of
  (VInt 0) -> VBool False
  (VInt _) -> VBool True
  (VReal 0) -> VBool False
  (VReal _) -> VBool True
  (VFloat 0) -> VBool False
  (VFloat _) -> VBool True
  (VString "") -> VBool False
  (VString _) -> VBool True
  (VNada) -> VBool False

evalOpUn ConvReal v = Just $ case v of
  (VInt x) -> VReal (fromIntegral x)
  (VFloat x) -> VReal (float2Double x)
  (VBool True) -> VReal 1
  (VBool False) -> VReal 0
  (VString s) -> VReal (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn ConvFloat v = Just $ case v of
  (VInt x) -> VFloat (fromIntegral x)
  (VReal x) -> VFloat (double2Float x)
  (VBool True) -> VFloat 1
  (VBool False) -> VFloat 0
  (VString s) -> VFloat (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn ConvString v = Just $ case v of
  (VInt x) -> VString $ show x
  (VReal x) -> VString $ show x
  (VBool b) -> VString $ show b
  VNada -> VString "nada"

instance Evaluavel Expr where
  eval (ELit l) = eval l
  eval (EVar id) = getVar id
  eval (EOpBin p op e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case evalOpBin op v1 v2 of
      Just v -> pure v
      Nothing -> throwError $ TypeError p

  eval (EOpUn p op e1) = do
    v1 <- eval e1
    case evalOpUn op v1 of
      Just v -> pure v
      Nothing -> throwError $ TypeError p

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
    return VNada
    where
      (ProcedimentoR pos i pars cs) = p
      (IdR _ nome) = i


      add :: [Parametro] -> [Valor] -> EvalM ()
      add ((Parametro n _ _) : ps) (v : vs) = addVar n v *> add ps vs
      add [] [] = pure ()
      add _ _ = throwError IncorrectNumberOfParameters

instance Evaluavel Programa where
  eval (Programa ps cs) = do
    modify $ \amb ->
      amb { ps = ps, escopo = "main" } 
    mapM_ eval cs
    return VNada

run :: EvalM (Valor) -> IO ()
run m = do
  (result, ambiente) <- runStateT (runExceptT m) ambienteVazio
  case result of
    Left err -> print err
    Right _ -> pure ()
  print ambiente
