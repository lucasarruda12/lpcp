module Repr where

import Lexer

data Pos = Pos 
  { inicio :: AlexPosn
  , fim    :: AlexPosn }
  deriving (Show)

class Positional a where
  getPos :: a -> Pos

mergePos :: (Positional a, Positional b) => a -> b -> Pos
mergePos a b = Pos s e
  where 
    (Pos s _) = getPos a
    (Pos _ e) = getPos b

data Id = IdR Pos String
  deriving (Show)

instance Positional Id where
  getPos (IdR p _) = p

type Tipo = Id

data Parametro = Parametro Id Tipo Bool -- o booleano indica se tem & ou nao (posso estar tendo uma ideia errada)
  deriving(Show)

--- ⚠⚠⚠  ☢☢parte que corrigi GERALDO OLHE ISSO CEGO ☢☢ ☣☣---

data Programa 
  = Programa = [Procedimento]
  deriving(Show)

data Procedimento  
  = Procedimento Pos Id [Parametro] [Comando]
  deriving(Show)

--- 🗿🗿🗿parte que corrigi GERALDO OLHE ISSO CEGO ⛔⛔⛔ ---
data Comando
  = Atribuicao Pos Id Expr 
  | Inicializacao Pos Id Tipo Expr
  | Declaracao Pos Id Tipo
  | SeCmd Pos Expr [Comando] [Comando] -- coloquei esse SeCmd para nao dar mais conflito entre o Lexer e o Repr
  | EnquantoCmd Pos Expr [Comando] -- mesma coisa aqui
  
  
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

data Lit
  = LInt Pos Int
  | LString Pos String
  | LBool Pos Bool
  deriving (Show)

instance Positional Lit where
  getPos (LInt p _) = p
  getPos (LString p _) = p
  getPos (LBool p _) = p

data Expr
  = ELit Lit
  | EVar Id
  | EChamada Pos Token [Expr]
  | EOpBin Pos OpBin Expr Expr
  | EOpUn Pos OpUn Expr
  deriving(Show)

instance Positional Expr where
  getPos (ELit l) = getPos l
  getPos (EVar (IdR p _)) = p
  getPos (EChamada p _ _) = p
  getPos (EOpBin p _ _ _) = p
  getPos (EOpUn p _ _) = p
-- ===================================

-- type Argumento = (Id, Descritor)
-- data Funcao = MkFuncao Id [Argumento] [Comando] Descritor
-- data Procedimento = MkProcedimento Id [Argumento] [Comando]
-- type Programa = [Comando]

