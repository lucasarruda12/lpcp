{
module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "//".*. ;
  $digit+ {\p s -> LitInt p (read s)}
  "::" {\p s -> QuatroPontos p}
  ":" {\p s -> DoisPontos p}
  "," {\p s -> Virgula p}
  "(" {\p s -> ParEsq p}
  ")" {\p s -> ParDir p}
  "&" {\p s -> EComercial p}
  "[" {\p s -> ColEsq p}
  "]" {\p s -> ColDir p}
  ";" {\p s -> PontoVirgula p}
  
  PROCEDIMENTO {\p s -> Procedimento p}
  FIM_PROCEDIMENTO. {\p s -> FimProcedimento p}
  ENQUANTO {\p s -> Enquanto p}
  FIM_ENQUANTO. {\p s -> FimEnquanto p}
  FAÇA {\p s -> Faca p}
  FIM_FAÇA. {\p s -> FimFaca p}
  INICIALIZE {\p s -> Inicialize p}
  INT {\p s -> Tipo p s}
  COM {\p s -> Com p}

  $alpha [$alpha $digit \_ \']* {\p s -> Id p s}

{

data Token
  = Procedimento AlexPosn   
  | FimProcedimento AlexPosn   
  | Enquanto AlexPosn   
  | FimEnquanto AlexPosn   
  | Faca AlexPosn   
  | FimFaca AlexPosn   
  | Inicialize AlexPosn   
  | Com AlexPosn   
  | Id AlexPosn String
  | Tipo AlexPosn String
  | ParEsq AlexPosn
  | ParDir AlexPosn
  | ColEsq AlexPosn
  | ColDir AlexPosn
  | QuatroPontos AlexPosn
  | DoisPontos AlexPosn
  | EComercial AlexPosn
  | Seta AlexPosn
  | PontoVirgula AlexPosn
  | Virgula AlexPosn
  | LitInt AlexPosn Int
  deriving (Eq, Show)

main :: IO ()
main = do
  s <- getContents
  print (alexScanTokens s)
}
