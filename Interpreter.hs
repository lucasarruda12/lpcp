module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

import Repr

data Valor 
  = VInt Int
  | VBool Bool
  | VString String
  | VNone

instance Show Valor where
  show (VInt x) = show x
  show (VBool b) = show b
  show (VString s) = show s
  show VNone = "None"

data Erro 
  = TypeError Pos
  | UndefinedVariable Pos
  | Context Pos Erro
  deriving (Show)

comPosicao :: Pos -> EvalM a -> EvalM a
comPosicao p m = 
  catchError m (\err -> throwError (Context p err))

type EvalM = ExceptT Erro (StateT Ambiente IO)

data Ambiente = Am
  { stackLocal :: Map.Map Id Valor
  , stackGlobal :: Map.Map Id Valor
  , heap :: Map.Map Int Valor 
  }
  deriving(Show)

getVar :: Id -> EvalM (Valor)
getVar id = do
  a <- get
  case Map.lookup id (stackLocal a) of
    Just v -> return v
    Nothing -> throwError $ UndefinedVariable (getPos id)

class Evaluavel a where
  eval :: a -> EvalM (Valor)

instance Evaluavel Lit where
  eval (LInt _ x) = pure $ VInt x
  eval (LBool _ b) = pure $ VBool b
  eval (LString _ s) = pure $ VString s

evalOpBin :: OpBin -> Valor -> Valor -> Maybe Valor
evalOpBin Soma (VInt x) (VInt y) = Just $ VInt (x + y)
evalOpBin Sub (VInt x) (VInt y) = Just $ VInt (x - y)
evalOpBin Mul (VInt x) (VInt y) = Just $ VInt (x * y)
evalOpBin Div (VInt x) (VInt y) = Just $ VInt (x `div` y)
evalOpBin Exp (VInt x) (VInt y) = Just $ VInt (x ^ y)
evalOpBin Mod (VInt x) (VInt y) = Just $ VInt (x `mod` y)
evalOpBin Menor (VInt x) (VInt y) = Just $ VBool (x < y)
evalOpBin Maior (VInt x) (VInt y) = Just $ VBool (x > y)
evalOpBin MenorIgualOp (VInt x) (VInt y) = Just $ VBool (x <= y)
evalOpBin MaiorIgualOp (VInt x) (VInt y) = Just $ VBool (x >= y)
evalOpBin IgualOp (VInt x) (VInt y) = Just $ VBool (x == y)
evalOpBin DiferenteOp (VInt x) (VInt y) = Just $ VBool (x /= y)
evalOpBin AndOp (VBool b1) (VBool b2) = Just $ VBool (b1 && b2)
evalOpBin OrOp (VBool b1) (VBool b2) = Just $ VBool (b1 || b2)
evalOpBin _ _ _ = Nothing

evalOpUn :: OpUn -> Valor -> Maybe Valor
evalOpUn Neg (VInt x) = Just $ VInt (-x)
evalOpUn NaoOp (VBool b) = Just $ VBool (not b)

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

  eval (Inicializacao p id t e) = comPosicao p $ do
    v <- eval e
    modify $ \amb ->
      amb { stackLocal = Map.insert id v (stackLocal amb) }
    return VNone

instance Evaluavel TopLevel where
  eval (TLComando c) = eval c

instance Evaluavel Programa where
  eval (Programa ls) = do
    mapM_ eval ls
    return VNone

run :: EvalM (Valor) -> IO ()
run m = do
  (result, ambiente) <- runStateT (runExceptT m) (Am Map.empty Map.empty Map.empty)
  case result of
    Left err -> print err
    Right v -> pure ()
  print ambiente
