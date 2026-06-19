module Repr where

import Lexer

data Pos = Pos 
  { inicio :: AlexPosn
  , fim    :: AlexPosn }
  deriving (Show, Eq, Ord)

class Positional a where
  getPos :: a -> Pos

mergePos :: (Positional a, Positional b) => a -> b -> Pos
mergePos a b = Pos s e
  where 
    (Pos s _) = getPos a
    (Pos _ e) = getPos b

data Id = IdR Pos String
  deriving (Show)

instance Eq Id where
  (IdR _ s) == (IdR _ s') = s == s'

instance Ord Id where
  (IdR _ s) <= (IdR _ s') = s <= s'

instance Positional Id where
  getPos (IdR p _) = p

data Tipo
  = TId Id
  | TList Tipo
  deriving (Show)

instance Positional Tipo where
  getPos (TId id) = getPos id
  getPos (TList t) = getPos t

data Parametro = Parametro Id Tipo Bool -- o booleano indica se tem & ou nao (posso estar tendo uma ideia errada)
  deriving(Show)
--- criei isso aqui tambem mas tem que conferir se está correto

data Programa = Programa
  { procedimentos :: [ProcedimentoR]
  -- Falta aqui:
  -- Funções
  -- Tipos definidos
  -- Mais (?)
  , comandos :: [Comando]
  }
  deriving (Show)

data ProcedimentoR
  = ProcedimentoR Pos Id [Parametro] [Comando]
  deriving(Show)

data Atribuendo
  = AId Id
  | AArray Id Expr
  | ARef Id
  deriving (Show)

instance Positional Atribuendo where
  getPos (AId id) = getPos id
  getPos (AArray id e) = mergePos id e
  getPos (ARef id) = getPos id

data Comando
  = Atribuicao Pos Atribuendo Expr
  | Inicializacao Pos Id Tipo Expr
  | Declaracao Pos Id Tipo
  | SeCmd Pos [(Expr, [Comando])]
  | EnquantoCmd Pos [(Expr, [Comando])]
  | Incremento Pos Id
  | ImprimaCmd Pos Expr
  | ChamadaCmd Pos Id [Expr]
  deriving (Show)

-- == Tudo relacionado a expressões ==
data OpBin
  = Soma | Sub | Mul | Div
  | Exp  | Mod | Menor | Maior
  | MenorIgualOp | MaiorIgualOp
  | IgualOp | DiferenteOp
  | AndOp | OrOp
  deriving (Show)

data OpUn
  = Neg | NaoOp | ConvInt | ConvBool
  | ConvReal | ConvString | ConvNada
  | ConvFloat

  deriving (Show)

data Lit
  = LInt Pos Int
  | LString Pos String
  | LBool Pos Bool
  | LFloat Pos Float
  | LReal Pos Double
  | LNada Pos
  deriving (Show)

instance Positional Lit where
  getPos (LInt p _) = p
  getPos (LString p _) = p
  getPos (LBool p _) = p

data Expr
  = ELit Lit
  | EVar Id
  | EChamada Pos Id [Expr]
  | EIndice Pos Expr Expr
  | EOpBin Pos OpBin Expr Expr
  | EOpUn Pos OpUn Expr
  deriving(Show)

instance Positional Expr where
  getPos (ELit l) = getPos l
  getPos (EVar (IdR p _)) = p
  getPos (EChamada p _ _) = p
  getPos (EIndice p _ _) = p
  getPos (EOpBin p _ _ _) = p
  getPos (EOpUn p _ _) = p
-- ===================================

-- type Argumento = (Id, Descritor)
-- data Funcao = MkFuncao Id [Argumento] [Comando] Descritor
-- data Procedimento = MkProcedimento Id [Argumento] [Comando]
-- type Programa = [Comando]
