module Parser where

import Lexer
import Repr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec (tokenPrim)
import Data.Maybe (Maybe(Nothing))
import Data.Maybe (isJust)
import Text.XHtml (start)

-- Adicionar os P depois dos parser -> parserP

type Parser a = Parsec [Token] () a

-- Tem muito token que eu não me importo com o resultado.
-- Procedimento, Fim_Procedimento... não me interessa o que que tem lá dentro
-- (até porque não tem nada)
-- Esse parser joga fora toda a informação de um token
-- (menos a posição)
tokenP :: TokenKind -> Parser AlexPosn
tokenP k = tokenPrim show nextPos test
   where
    test t@(Token p k')
      | k == k'   = Just p
      | otherwise = Nothing
    nextPos pos _ _     = pos

idP :: Parser Id
idP = tokenPrim show nextPos test
  where
    test (Token p (Id s)) = Just (IdR (Pos p p) s)
    test _ = Nothing
    nextPos pos _ _ = pos

litP :: Parser Lit
litP = litIntP <|> litStringP <|> litBoolP
  where
    nextPos pos _ _ = pos

    litIntP = tokenPrim show nextPos testInt
    testInt (Token p (LitInt x)) = Just (LInt (Pos p p) x)
    testInt _ = Nothing

    litStringP = tokenPrim show nextPos testString
    testString (Token p (LitString s)) = Just (LString (Pos p p) s)
    testString _ = Nothing

    -- Falta ter tokens para booleanos

    litBoolP = tokenPrim show nextPos testBool

    testBool (Token p (LitBool b)) = Just (LBool (Pos p p) b)
    testBool _ = Nothing


parens :: Parser a -> Parser a
parens p = do
  tokenP ParEsq
  t <- p
  tokenP ParDir
  return t
 
-- Isso não tá legal
-- ⛔⛔⛔ LUCAS OLHA AQUI ⛔⛔⛔
-- Parsear um Id e depois levar pros constrututores corretos de Tipo
tipoP :: Parser Tipo
tipoP = TId <$> idP <|> (do
  tokenP ColEsq
  t <- tipoP
  tokenP ColDir
  return $ TList t)

atribuendoP :: Parser Atribuendo
atribuendoP =
      try (AId <$> IdP) 
  <|> try (AArray <$> Id <*> Expr) 
  <|> try (ARed <$> Id <* TokenP Vezes)

 
-- Usa esse aqui como exemplo!
atribuicao :: Parser Comando
atribuicao = do
  lvalue <- atribuendoP
  tokenP Igual
  e <- exprP
  tokenP PontoVirgula
  return (Atribuicao (mergePos lvalue e)  lvalue e)

inicializacao :: Parser Comando
inicializacao = do
  start <- tokenP Inicialize
  id <- idP
  tokenP QuatroPontos
  t <- tipoP
  tokenP Com
  e <- exprP
  end <- tokenP PontoVirgula
  return (Inicializacao (Pos start end) id t e)
 
declaracao :: Parser Comando
declaracao = do
  start <- tokenP Declare
  id <- idP
  tokenP QuatroPontos
  t <- tipoP
  end <- tokenP PontoVirgula
  return (Declaracao (Pos start end) id t)

-- Sobre se e enquanto:
-- Vamos usar as construções de Dijskra mesmo?
-- Se sim, isso aqui não vai funcionar
se :: Parser Comando
se = do
  start <- tokenP Se
  cond <- exprP
  tokenP DoisPontos
  cmdsThen <- many comando
  tokenP Senao
  tokenP DoisPontos
  cmdsElse <- many comando

  end <- tokenP FimSe
  return(SeCmd (Pos start end) cond cmdsThen cmdsElse)

enquanto :: Parser Comando
enquanto = do
  start <- tokenP Enquanto
  tokenP DoisPontos
  cond <- exprP
  tokenP Faca
  tokenP DoisPontos
  cmdsThen <- many comando
  tokenP FimFaca
  end <- tokenP FimEnquanto
  return(EnquantoCmd (Pos start end) cond cmdsThen)


--- ⛔⛔⛔ ⚠⚠⚠CRIA O PARSER DO PROGRAMA AQUI ☣☣☣⛔⛔⛔---
-- Porque não chamar de programaP?
topLevelP :: Parser TopLevel
topLevelP = 
    (TLProcedimento <$> procedimentoP)
   <|> (TLComando <$> comando)

programa :: Parser Programa
programa = do
  tls <- many topLevelP
  eof
  return (Programa tls)  


procedimentoP :: Parser ProcedimentoR
procedimentoP = do
  start <- tokenP Procedimento
  id <- idP
  paresq <- tokenP ParEsq
  parametros <- sepBy parametroP (tokenP Virgula)
  pardir <- tokenP ParDir
  tokenP DoisPontos
  cmds <- many comando
  end <- tokenP FimProcedimento
  return (ProcedimentoR (Pos start end) id parametros cmds)

comando :: Parser Comando
comando =
   try (incremento)
   <|> try (atribuicao)
   <|> inicializacao
   <|> declaracao
   <|> se
   <|> enquanto

-- Isso aqui não vai funcionar
parametroP :: Parser Parametro
parametroP = do
  ref <- optionMaybe(tokenP EComercial)
  id <- idP
  tokenP QuatroPontos
  t <- tipoP
  return(Parametro id t (isJust ref))

chamadaP :: Parser Expr
chamadaP = do
  f <- idP
  tokenP ParEsq
  args <- sepBy exprP (tokenP Virgula)
  tokenP ParDir
  return (EChamada (getPos f) f args)

indiceP :: Parser Expr
indiceP = do
  v <- idP
  tokenP ColEsq
  idx <- exprP
  tokenP ColDir
  return (EIndice (mergePos v idx) (EVar v) idx)

incremento = do
  id <- idP
  tokenP MaisMais
  tokenP PontoVirgula
  return (Incremento (getPos id) id)

-- Documentação do buildExpressionParser:
-- https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Expr.html
exprP :: Parser Expr
exprP = buildExpressionParser table term
  where
    table =
      [ [ Infix (binOpP VezesVezes Exp) AssocRight
        ]
      , [ Infix (binOpP Vezes Mul) AssocLeft
        , Infix (binOpP Divide Div) AssocLeft
        , Infix (binOpP Porcento Mod) AssocLeft
        ]
      , [ Infix (binOpP Mais Soma) AssocLeft
        , Infix (binOpP Menos Sub) AssocLeft
        ]
      , [ Prefix (unOpP Menos Neg)
        , Prefix (unOpP NaoLogico NaoOp)
        ] 
      , [ Infix (binOpP MenorQue Menor) AssocNone
        , Infix (binOpP MaiorQue Maior) AssocNone
        , Infix (binOpP MenorIgual MenorIgualOp) AssocNone
        , Infix (binOpP MaiorIgual MaiorIgualOp) AssocNone
        ]
      , [ Infix (binOpP IgualIgual IgualOp) AssocNone
        , Infix (binOpP Diferente DiferenteOp) AssocNone
        ]
      , [ Infix (binOpP ELogico AndOp) AssocLeft
        ] 
      , [ Infix (binOpP OuLogico OrOp) AssocLeft
        ]   
      ]

    binOpP tk op = do
      tokenP tk
      pure $ \l r -> 
        EOpBin (mergePos l r) op l r

    unOpP tk op = do
      tokenP tk
      pure $ \e -> EOpUn (getPos e) op e

    term =
        try chamadaP
        <|> try indiceP
        <|> ELit <$> litP
        <|> EVar <$> idP
        <|> parens exprP
-- TEM QUE IDENTAR GERALDO
