{
module Lexer (Token(..), TokenKind(..), tokenize, position, kind, AlexPosn(..)) where
}
%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "//".*. ;
  $digit+ {\p s -> Token p (LitInt (read s))}
  "::" {\p _ -> Token p QuatroPontos}
  ":" {\p _ -> Token p DoisPontos}
  "," {\p _ -> Token p Virgula}
  "(" {\p _ -> Token p ParEsq}
  ")" {\p _ -> Token p ParDir}
  "&" {\p _ -> Token p EComercial}
  "[" {\p _ -> Token p ColEsq}
  "]" {\p _ -> Token p ColDir}
  "{" {\p _ -> Token p ChaveEsq}
  "}" {\p _ -> Token p ChaveDir}
  ";" {\p _ -> Token p PontoVirgula}
  "->" {\p _ -> Token p Seta}
  "<=" {\p _ -> Token p MenorIgual}
  ">=" {\p _ -> Token p MaiorIgual}
  "<" {\p _ -> Token p MenorQue}
  ">" {\p _ -> Token p MaiorQue}
  "=" {\p _ -> Token p Igual}
  "==" {\p _ -> Token p IgualIgual}
  "!=" {\p _ -> Token p Diferente}
  "++" {\p _ -> Token p MaisMais}
  "+" {\p _ -> Token p Mais}
  "-" {\p _ -> Token p Menos}
  "/" {\p _ -> Token p Divide}
  "|" {\p _ -> Token p Pipe}
  "%" {\p _ -> Token p Porcento}
  "*" {\p _ -> Token p Vezes}
  "**" {\p _ -> Token p VezesVezes}
  "." {\p _ -> Token p Ponto}
  
  PROCEDIMENTO {\p _ -> Token p Procedimento}
  FIM_PROCEDIMENTO. {\p _ -> Token p FimProcedimento}
  FUNCAO {\p _ -> Token p Funcao}
  FIM_FUNCAO. {\p _ -> Token p FimFuncao}
  RETORNE {\p _ -> Token p Retorne}
  ENQUANTO {\p _ -> Token p Enquanto}
  FIM_ENQUANTO. {\p _ -> Token p FimEnquanto}
  FAÇA {\p _ -> Token p Faca}
  SE {\p _ -> Token p Se}
  SENÃO {\p _ -> Token p Senao}
  SENAO {\p _ -> Token p Senao}
  FIM_FAÇA. {\p _ -> Token p FimFaca}
  FIM_FACA. {\p _ -> Token p FimFaca}
  FIM_SE. {\p _ -> Token p FimSe}
  INICIALIZE {\p _ -> Token p Inicialize}
  DECLARE {\p _ -> Token p Declare}
  COM {\p _ -> Token p Com}
  IMPRIMA {\p _ -> Token p Imprima}
  NADA {\p _ -> Token p Nada}

  ENUM {\p _ -> Token p EnumTok}
  FIM_ENUM {\p _ -> Token p FimEnum}
  CASAMENTO { \p _ -> Token p CasamentoTok}
  FIM_CASAMENTO { \p _ -> Token p FimCasamento}

  ESTRUTURA {\p _ -> Token p Estrutura}
  FIM_ESTRUTURA. {\p _ -> Token p FimEstrutura}
  
  -- Tipos
  int {\p _ -> Token p Int}
  float {\p _ -> Token p Float}
  real {\p _ -> Token p Real}
  bool {\p _ -> Token p Bool}
  string {\p _ -> Token p String}

  -- Leia
  leia {\p _ -> Token p Leia}

  -- adicionando BOOLS --
  VERDADEIRO {\p _ -> Token p (LitBool True)}
  FALSO      {\p _ -> Token p (LitBool False)}

  -- STRINGS --
  \"([^\"\\]|\\.)*\" { \p s -> Token p (LitString (init (tail s))) }

  -- FLOATS E REAIS --
  [0-9]+\.[0-9]+r { \p s -> Token p (LitReal (read . init $ s)) }
  [0-9]+\.[0-9]+ {\p s -> Token p (LitFloat (read s)) }

  -- OPERADORES LOGICOS
  AND    {\p _ -> Token p ELogico}
  OR   {\p _ -> Token p OuLogico}
  NOT  {\p _ -> Token p NaoLogico}

  $alpha [$alpha $digit \_ \']* {\p s -> Token p (Id s)}
{
-- Record Syntax: 
-- devtut.github.io/haskell/record-syntax.html
data Token = Token AlexPosn TokenKind

instance Show Token where
  show (Token _ tk) = show tk

position :: Token -> (Int, Int, Int)
position (Token (AlexPn x y z) _) = (x, y, z)

kind :: Token -> TokenKind
kind (Token _ k) = k

-- Não quero derivar o Show,
-- porque se não os tokens só vão ser iguais se tiverem a mesma posição!
instance Eq Token where
  t1 == t2 = kind t1 == kind t2

data TokenKind
  = Procedimento    
  | FimProcedimento    
  | Funcao
  | FimFuncao
  | Retorne
  | Imprima
  | Enquanto    
  | FimEnquanto    
  | Faca    
  | FimFaca    
  | Se    
  | Senao    
  | FimSe    
  | Inicialize    
  | Com    
  | Id String
  | Tipo String
  | ParEsq 
  | ParDir 
  | ColEsq 
  | ColDir 
  | ChaveEsq
  | ChaveDir
  | QuatroPontos 
  | DoisPontos 
  | EComercial 
  | Seta 
  | PontoVirgula 
  | Virgula 
  | LitInt Int
  | LitFloat Float
  | LitReal Double
  | LitBool Bool
  | LitString String
  | ELogico
  | OuLogico
  | NaoLogico
  | MenorQue  
  | Igual 
  | Ponto 
  | IgualIgual 
  | Diferente 
  | Mais 
  | Menos 
  | Divide 
  | Porcento 
  | Vezes 
  | VezesVezes 
  | MaisMais 
  | MaiorQue 
  | MaiorIgual 
  | MenorIgual 
  | AspasDuplas 
  | Pipe
  | Declare
  | Nada
  | Int
  | Float
  | Real
  | Bool
  | String
  | Leia
  | EnumTok
  | FimEnum
  | CasamentoTok
  | FimCasamento
  | Estrutura
  | FimEstrutura
  deriving (Eq)

instance Show TokenKind where
  show Procedimento = "PROCEDIMENTO"
  show FimProcedimento = "FIM_PROCEDIMENTO."
  show Funcao = "FUNCAO"
  show FimFuncao = "FIM_FUNCAO."
  show Retorne = "RETORNE"
  show Imprima = "IMPRIMA"
  show Enquanto = "ENQUANTO"
  show FimEnquanto = "FIM_ENQUANTO."
  show Faca ="FAÇA"
  show FimFaca = "FIM_FAÇA."
  show Se = "SE"
  show Senao = "SENÃO"
  show FimSe = "FIM_SE."
  show Inicialize = "INICIALIZE"
  show Com = "COM"
  show (Id s) = s
  show (Tipo s) = s
  show ParEsq ="("
  show ParDir = ")"
  show ColEsq = "["
  show ColDir = "]"
  show ChaveEsq = "{"
  show ChaveDir = "}"
  show QuatroPontos = "::"
  show DoisPontos = ":"
  show EComercial = "&"
  show Seta = "->"
  show PontoVirgula = ";"
  show Virgula = ","
  show (LitInt x) = show x
  show (LitFloat f) = show f
  show (LitReal d) = show d
  show (LitBool b) = show b
  show (LitString s) = s
  show ELogico = "AND"
  show OuLogico = "OR"
  show NaoLogico = "NOT"
  show MenorQue = "<"
  show Igual = "="
  show Ponto = "."
  show IgualIgual = "=="
  show Diferente = "!="
  show Mais = "+"
  show Menos = "-"
  show Divide = "/"
  show Porcento = "%"
  show Vezes = "*"
  show VezesVezes = "**"
  show MaisMais = "++"
  show MaiorQue = ">"
  show MaiorIgual = ">="
  show MenorIgual = "<=" 
  show AspasDuplas = "\"\""
  show Pipe= "|"
  show Declare= "DECLARE"
  show Nada= "NADA"
  show Int= "int"
  show Float= "float"
  show Real= "real"
  show Bool= "bool"
  show String= "string"
  show Leia= "leia"
  show EnumTok="ENUM"
  show FimEnum="FIM_ENUM."
  show CasamentoTok="CASAMENTO"
  show FimCasamento="FIM_CASAMENTO."
  show Estrutura="ESTRUTURA"
  show FimEstrutura="FIM_ESTRUTURA."
   
tokenize :: String -> [Token]
tokenize = alexScanTokens
}
