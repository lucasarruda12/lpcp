{
module Lexer (Token(..), TokenKind(..), tokenize, position, kind) where
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
  INT {\p s -> Token p (Tipo s)}
  COM {\p _ -> Token p Com}
  \"([^\"\\]|\\.)*\" { \p s -> Token p (LitString s)}

  $alpha [$alpha $digit \_ \']* {\p s -> Token p (Id s)}
{
-- Record Syntax: 
-- devtut.github.io/haskell/record-syntax.html
data Token = Token AlexPosn TokenKind
  deriving (Show)

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
  | QuatroPontos 
  | DoisPontos 
  | EComercial 
  | Seta 
  | PontoVirgula 
  | Virgula 
  | LitInt Int
  | LitBool Bool
  | LitString String
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
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
