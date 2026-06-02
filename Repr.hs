module Repr where

import Lexer

type Id = String
type Argumento = (Id, Descritor)
data Funcao = MkFuncao Id [Argumento] [Comando] Descritor
data Procedimento = MkProcedimento Id [Argumento] [Comando]

type Programa = [Comando]

data Descritor
  = DInt Int

data Comando 
  = Atribuicao Token Expr 
  | Inicializacao Token Token Expr
  | Declaracao Token Token
  deriving (Show)

-- == Tudo relacionado a expressões ==
data OpBin
  = Soma
  | Sub
  | Mul
  | Div
  | Exp
  | Mod
  deriving (Show)

data OpUn
  = Neg
  deriving (Show)

data Expr 
  = EInt Token
  | EVar Token
  | EString Token
  | EChamada Token [Expr]
  | EOpBin OpBin Expr Expr
  | EOpUn OpUn Expr
  deriving(Show)
-- ===================================
