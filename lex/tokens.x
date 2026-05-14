{
module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+ ;
  "//".*. ;
  $digit+ {\p s -> LitInt p (read s)}
  "::" {\p s -> QuatroPontos p}
  $alpha [$alpha $digit \_ \']* {\p s -> Id p s}
  
  PROCEDIMENTO {\p s -> Procedimento p}
  FIM_PROCEDIMENTO {\p s -> FimProcedimento p}
  INICIALIZE {\p s -> Inicialize p}
  INT {\p s -> Tipo p s}
  COM {\p s -> Com p}

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

}
