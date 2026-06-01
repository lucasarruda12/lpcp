module Program where

import Lexer

type Id = String
type Argumento = (Id, Descritor)
data Funcao = MkFuncao Id [Argumento] [Comando] Descritor
data Procedimento = MkProcedimento Id [Argumento] [Comando]

type Programa = [Comando]

data Descritor
  = DInt Int

data Comando 
  = Atribuicao Id Expr 
  | Inicializacao Id Expr

data Expr 
  = Lit Token
  | EVar Id
  | EChamada Id [Expr]
  | ESoma Expr Expr
  deriving(Show)


