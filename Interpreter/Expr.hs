module Interpreter.Expr where

import Control.Monad.State
import Control.Monad.Except

import GHC.Float (float2Double, double2Float)

import Interpreter.Basic
import Interpreter.Erro
import System.IO (getLine)

import Repr

instance Evaluavel Lit where
  eval (LInt _ x) = pure $ VInt x
  eval (LBool _ b) = pure $ VBool b
  eval (LString _ s) = pure $ VString s
  eval (LNada _) = pure $ VNada
  eval (LReal _ r) = pure $ VReal r
  eval (LFloat _ f) = pure $ VFloat f

instance Evaluavel Expr where
  eval (ELit l) = eval l
  eval (EVar id) = getVar id

  eval (ELeia p) = VString <$> liftIO getLine

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
