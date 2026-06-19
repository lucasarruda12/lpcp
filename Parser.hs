module Parser where

import Lexer
import Repr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec (tokenPrim)
import Data.Maybe (Maybe(Nothing))
import Data.Maybe (isJust)
import Text.XHtml (start)

type Parser a = Parsec [Token] () a

---------------------------------------
-- Helpers ----------------------------
parens :: Parser a -> Parser a
parens p = do
  tokenP ParEsq
  t <- p
  tokenP ParDir
  return t

-- Joga fora o Token
tokenP :: TokenKind -> Parser AlexPosn
tokenP k = tokenPrim show nextPos test
   where
    test t@(Token p k')
      | k == k'   = Just p
      | otherwise = Nothing
    nextPos pos _ _     = pos
---------------------------------------
---------------------------------------

idP :: Parser Id
idP = tokenPrim show nextPos test
  where
    test (Token p (Id s)) = Just (IdR (Pos p p) s)
    test _ = Nothing
    nextPos pos _ _ = pos

litP :: Parser Lit
litP = litIntP <|> litStringP <|> litBoolP <|> litFloatP <|> litRealP <|> litNadaP
  where
    nextPos pos _ _ = pos

    litIntP = tokenPrim show nextPos testInt
    testInt (Token p (LitInt x)) = Just (LInt (Pos p p) x)
    testInt _ = Nothing

    litStringP = tokenPrim show nextPos testString
    testString (Token p (LitString s)) = Just (LString (Pos p p) s)
    testString _ = Nothing

    litBoolP = tokenPrim show nextPos testReal
    testBool (Token p (LitBool b)) = Just (LBool (Pos p p) b)
    testBool _ = Nothing

    litRealP = tokenPrim show nextPos testReal
    testReal (Token p (LitReal r)) = Just (LReal (Pos p p) r)
    testReal _ = Nothing

    litFloatP = tokenPrim show nextPos testFloat
    testFloat (Token p (LitFloat f)) = Just (LFloat (Pos p p) f)
    testFloat _ = Nothing

    litNadaP = do
      p <- tokenP Nada
      return (LNada (Pos p p))
 
tipoP :: Parser Tipo
tipoP = TId <$> idP 
  <|> (do
    tokenP ColEsq
    t <- tipoP
    tokenP ColDir
    return $ TList t)

---------------------------------------
-- Tudo sobre Comandos ----------------
comandoP :: Parser Comando
comandoP =
  try incrementoP
  <|> try atribuicaoP
  <|> imprimaP
  <|> inicializacaoP
  <|> declaracaoP
  <|> seP
  <|> enquantoP
  <|> chamadaCmdP
    where
      incrementoP :: Parser Comando
      incrementoP = do
        id <- idP
        tokenP MaisMais
        tokenP PontoVirgula
        return (Incremento (getPos id) id)
      
      atribuicaoP :: Parser Comando
      atribuicaoP = do
        lvalue <- atribuendoP
        tokenP Igual
        e <- exprP
        tokenP PontoVirgula
        return (Atribuicao (mergePos lvalue e)  lvalue e)

      atribuendoP :: Parser Atribuendo
      atribuendoP =
            try (AId <$> idP) 
        <|> try (AArray <$> idP <*> exprP) 
        <|> try (ARef <$> idP <* tokenP Vezes)

      imprimaP :: Parser Comando
      imprimaP = do
        start <- tokenP Imprima
        e <- exprP
        end <- tokenP PontoVirgula
        return (ImprimaCmd (Pos start end) e)

      inicializacaoP :: Parser Comando
      inicializacaoP = do
        start <- tokenP Inicialize
        id <- idP
        tokenP QuatroPontos
        t <- tipoP
        tokenP Com
        e <- exprP
        end <- tokenP PontoVirgula
        return (Inicializacao (Pos start end) id t e)
      
      declaracaoP :: Parser Comando
      declaracaoP = do
        start <- tokenP Declare
        id <- idP
        tokenP QuatroPontos
        t <- tipoP
        end <- tokenP PontoVirgula
        return (Declaracao (Pos start end) id t)
      
      seP :: Parser Comando
      seP = do
        start <- tokenP Se
        tokenP DoisPontos
        unc <- many unidadeCondicionalP
        end <- tokenP FimSe
        return (SeCmd (Pos start end) unc)

      unidadeCondicionalP :: Parser (Expr, [Comando])
      unidadeCondicionalP = do
        cond <- exprP
        tokenP Virgula
        tokenP Faca
        tokenP DoisPontos
        cmds <- many comandoP
        tokenP FimFaca
        return (cond, cmds)
      
      enquantoP :: Parser Comando
      enquantoP = do
        start <- tokenP Enquanto
        tokenP DoisPontos
        unc <- many unidadeCondicionalP
        end <- tokenP FimEnquanto
        return (EnquantoCmd (Pos start end) unc)

      chamadaCmdP :: Parser Comando
      chamadaCmdP = do
        p <- idP
        args <- parens $ sepBy exprP (tokenP Virgula)
        tokenP PontoVirgula
        return (ChamadaCmd (getPos p) p args)
---------------------------------------
---------------------------------------
adicionarProcedimento :: ProcedimentoR -> Programa -> Programa
adicionarProcedimento p (Programa ps cs) = Programa (p : ps) cs

adicionarComando :: Comando -> Programa -> Programa
adicionarComando c (Programa ps cs) = Programa ps (c : cs)

programaP :: Parser Programa
programaP
  =   (adicionarProcedimento <$> procedimentoP <*> programaP)
  <|> (adicionarComando <$> comandoP <*> programaP) 
  <|> (pure (Programa [] []) <* eof)
  where
    procedimentoP :: Parser ProcedimentoR
    procedimentoP = do
      start <- tokenP Procedimento
      id <- idP
      paresq <- tokenP ParEsq
      parametros <- sepBy parametroP (tokenP Virgula)
      pardir <- tokenP ParDir
      tokenP DoisPontos
      cmds <- many comandoP
      end <- tokenP FimProcedimento <?> "FIM_PROCEDIMENTO."
      return (ProcedimentoR (Pos start end) id parametros cmds)

-- Isso aqui não vai funcionar
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

    convP :: Parser Expr
    convP = 
      choice
        [ try $ convP' op tok
        | (op, tok) <-
            [ (ConvInt, TInt)
            , (ConvReal, TReal)
            , (ConvBool, TBool)
            , (ConvNada, Nada)
            , (ConvString, TString)
            , (ConvFloat, TFloat)
            ]
        ]

    convP' op tok = do
      start <- tokenP tok
      e <- parens exprP
      return $ EOpUn (getPos e) op e

    term =
        try chamadaP
        <|> try indiceP
        <|> ELit <$> litP
        <|> EVar <$> idP
        <|> convP
        <|> (do p <- tokenP Leia; return (ELeia (Pos p p)))
        <|> parens exprP
