module Repr where

import Lexer
import Data.List (intercalate)

newtype Pos = Pos AlexPosn
  deriving (Eq, Ord)

base :: Pos
base = Pos $ AlexPn 0 0 0

instance Show Pos where
  show (Pos (AlexPn _ l c)) = "[l" ++ show l ++ ":c" ++ show c ++ "]"

class Positional a where
  getPos :: a -> Pos

data Id = IdR Pos String

instance Show Id where
  show (IdR _ nome) = nome

instance Eq Id where
  (IdR _ s) == (IdR _ s') = s == s'

instance Ord Id where
  (IdR _ s) <= (IdR _ s') = s <= s'

instance Positional Id where
  getPos (IdR p _) = p

data Tipo
  = TId Id
  | TInt
  | TString
  | TFloat
  | TBool
  | TReal
  | TNada
  | TList Tipo
  | TTuple [Tipo]
  | TDict Tipo Tipo
  | TMatrix Tipo Int Int
  | TQualquer
  deriving (Eq, Ord)

data Parametro = Parametro Id Tipo Bool -- o booleano indica se tem & ou nao (posso estar tendo uma ideia errada)

instance Show Parametro where
  show (Parametro ident tipo False) = show ident ++ " :: " ++ show tipo
  show (Parametro ident tipo True) = "&" ++ show ident ++ " :: " ++ show tipo
--- criei isso aqui tambem mas tem que conferir se está correto

data Programa = Programa
  { procedimentos :: [ProcedimentoR]
  , funcoes       :: [FuncaoR]
  , enums         :: [EnumDecl]
  , estruturas    :: [EstruturaDecl]
  , comandos      :: [Comando]
  }
  deriving (Show)

data ProcedimentoR
  = ProcedimentoR Pos Id [Parametro] [Comando]

instance Positional ProcedimentoR where
  getPos (ProcedimentoR p _ _ _) = p

instance Show ProcedimentoR where
  show (ProcedimentoR _ nome pars _) 
    = show nome ++ "(" ++ intercalate ", " (show <$> pars) ++ ")"

data FuncaoR 
  = FuncaoR Pos Id [Parametro] Tipo [Comando]

instance Show FuncaoR where
  show (FuncaoR _ nome pars tipo _) = show nome ++ "(" ++ intercalate ", " (show <$> pars) ++ ") -> " ++ show tipo

instance Positional FuncaoR where
  getPos (FuncaoR p _ _ _ _) = p

data Atribuendo
  = AId Id
  | AArray Id Expr
  | AEstrutura Id Id

instance Show Atribuendo where
  show (AId ident) = show ident
  show (AArray ident idx) = show ident ++ "[" ++ show idx ++ "]"
  show (AEstrutura ident campo) = show ident ++ "." ++ show campo

instance Positional Atribuendo where
  getPos (AId ident) = getPos ident
  getPos (AArray ident _) = getPos ident
  getPos (AEstrutura ident _) = getPos ident

data Comando
  = Atribuicao Pos Atribuendo Expr
  | Inicializacao Pos Id Tipo Expr
  | Declaracao Pos Id Tipo
  | SeCmd Pos [(Expr, [Comando])]
  | EnquantoCmd Pos [(Expr, [Comando])]
  | Incremento Pos Id
  | ImprimaCmd Pos Expr
  | RetorneCmd Pos Expr
  | ChamadaCmd Pos Id [Expr]
  | CasamentoCmd Pos Expr [(Padrao, [Comando])]
  | PasseCmd Pos

instance Positional Comando where
  getPos (Atribuicao p _ _) = p
  getPos (Inicializacao p _ _ _) = p
  getPos (Declaracao p _ _) = p
  getPos (SeCmd p _) = p
  getPos (EnquantoCmd p _) = p
  getPos (ImprimaCmd p _) = p
  getPos (ChamadaCmd p _ _) = p
  getPos (Incremento p _) = p
  getPos (RetorneCmd p _) = p
  getPos (CasamentoCmd p _ _) = p
  getPos (PasseCmd p) = p

instance Show Comando where
  show (Atribuicao _ lvalue rvalue) = show lvalue ++ " = " ++ show rvalue
  show (Inicializacao _ nome tipo rvalue) 
    = "INICIALIZE " ++ show nome ++ " :: " ++ show tipo ++ " COM " ++ show rvalue
  show (Declaracao _ nome tipo) = "DECLARE" ++ show nome ++ " :: " ++ show tipo
  show (SeCmd _ _) = "SE"
  show (EnquantoCmd _ _) = "ENQUANTO"
  show (ImprimaCmd _ nome) = "IMPRIMA " ++ show nome
  show (RetorneCmd _ nome) = "RETORNE " ++ show nome
  show (ChamadaCmd _ nome pars) = show nome ++ "(" ++ intercalate ", " (show <$> pars) ++ ")"
  show (CasamentoCmd _ _ _) = "CASAMENTO" -- tava faltando isso
  show (Incremento _ nome) = show nome ++ "++"
  show (PasseCmd p) = "PASSE"

-- == Tudo relacionado a expressões ==
data OpBin
  = Soma | Sub | Mul | Div
  | Exp  | Mod | Menor | Maior
  | MenorIgualOp | MaiorIgualOp
  | IgualOp | DiferenteOp
  | AndOp | OrOp
  deriving (Eq, Ord)

instance Show OpBin where
  show Soma = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "**"
  show Mod = "%"
  show Menor = "<"
  show Maior = ">"
  show MenorIgualOp = "<="
  show MaiorIgualOp = ">="
  show IgualOp = "=="
  show DiferenteOp = "!="
  show AndOp = "AND"
  show OrOp = "Or"

data OpUn
  = Neg | NaoOp | Conv Tipo

instance Show OpUn where
  show Neg = "-"
  show NaoOp = "NAO"
  show (Conv t) = show t

data Lit
  = LInt Pos Int
  | LString Pos String
  | LBool Pos Bool
  | LFloat Pos Float
  | LReal Pos Double
  | LNada Pos

instance Show Lit where
  show (LInt _ x) = show x
  show (LString _ x) = show x
  show (LBool _ x) = show x
  show (LFloat _ x) = show x
  show (LReal _ x) = show x
  show (LNada _) = "NADA"

data Padrao = Padrao Id (Maybe Id)

data VarianteEnum = VarianteEnum Id (Maybe Tipo)

instance Show VarianteEnum where
  show (VarianteEnum id Nothing) = show id
  show (VarianteEnum id (Just t)) = show id ++ "(" ++ show t ++ ")"

data EnumDecl = EnumDecl Pos Id [VarianteEnum]

instance Show EnumDecl where
  show (EnumDecl _ id _) = "enum " ++ show id

data EstruturaDecl = EstruturaDecl Pos Id [(Id, Tipo)]

instance Show EstruturaDecl where
  show (EstruturaDecl _ id _) = "estrutura " ++ show id


instance Positional Lit where
  getPos (LInt p _) = p
  getPos (LString p _) = p
  getPos (LBool p _) = p
  getPos (LFloat p _) = p
  getPos (LReal p _) = p
  getPos (LNada p) = p

data Expr
  = ELit Lit
  | EVar Id
  | ELeia Pos
  | EChamada Pos Id [Expr]
  | EIndice Pos Expr Expr
  | EOpBin Pos OpBin Expr Expr
  | EOpUn Pos OpUn Expr
  | EList Pos [Expr]
  | ETuple Pos [Expr]
  | EDict Pos [(Expr, Expr)]
  | EEnum Pos Id Id (Maybe Expr)
  | EAcesso Pos Id Id
  | EEstrutura Pos Id [(Id, Expr)]

instance Show Expr where
  show (ELit l) = show l
  show (EVar v) = show v
  show (ELeia _) = "leia"
  show (EOpBin _ op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (EOpUn _ op e) = show op ++ show e
  show (EEnum _ enum variante (Just e)) = show enum ++ "::" ++ show variante ++ "(" ++ show e ++ "("
  show (EEnum _ enum variante Nothing) = show enum ++ "::" ++ show variante
  show (EAcesso _ est campo) = show est ++ "." ++ show campo
  show (EEstrutura _ nome campos) = "{" ++ show campos ++ "}"
  show (EChamada _ f args) = show f ++ "(" ++ intercalate "," (map show args) ++ ")"
  show (EIndice _ e idx) = show e ++ "[" ++ show idx ++ "]"
  show (EList _ es) = "[" ++ intercalate "," (map show es) ++ "]"
  show (ETuple _ es) = "{" ++ intercalate "," (map show es) ++ "}"
  show (EDict _ campos) = 
    "{" ++ intercalate "," (map show campos) ++ "}"

instance Positional Expr where
  getPos (ELit l) = getPos l
  getPos (EVar (IdR p _)) = p
  getPos (EChamada p _ _) = p
  getPos (EIndice p _ _) = p
  getPos (EOpBin p _ _ _) = p
  getPos (EOpUn p _ _) = p
  getPos (EList p _) = p
  getPos (ETuple p _) = p
  getPos (EDict p _) = p
  getPos (ELeia p) = p
  getPos (EEnum p _ _ _) = p
  getPos (EAcesso p _ _) = p
  getPos (EEstrutura p _ _) = p
-- ===================================

---------------------------------------
-- Shows: -----------------------------
instance Show Tipo where
  show (TId nome) = show nome
  show TInt = "Int"
  show TString = "String"
  show TFloat = "Float"
  show TBool = "Bool"
  show TReal = "Real"
  show (TList tipo) = "[" ++ show tipo ++ "]"
  show (TTuple tipos) = "(" ++ intercalate ", " (show <$> tipos) ++ ")"
  show (TDict t1 t2) = "{" ++ show t1 ++ "," ++ show t2 ++ "}"
  show (TMatrix t r c) = show t ++ "[" ++ show r ++ "x" ++ show c ++ "]"
  show TNada = "Nada"
  show TQualquer = "Qualquer"
