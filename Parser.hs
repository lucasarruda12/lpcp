module Parser where

import Lexer
import Repr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Pos (newPos)
import Data.Maybe (isJust)
import Data.Char (isDigit)
import Distribution.SPDX (SimpleLicenseExpression)

type Parser a = Parsec [Token] () a

---------------------------------------
-- Helpers ----------------------------
parens :: Parser a -> Parser a
parens p = tokenP ParEsq *> p <* tokenP ParDir

updatePos :: SourcePos -> Token -> s -> SourcePos
updatePos sp (Token (AlexPn _ line col) _) _ =
  newPos (sourceName sp) line col

-- Joga fora o Token
tokenP :: TokenKind -> Parser Pos
tokenP k = tokenPrim show updatePos test
   where
    test t@(Token p k')
      | k == k'   = Just (Pos p)
      | otherwise = Nothing
---------------------------------------
---------------------------------------

idP :: Parser Id
idP = tokenPrim show updatePos test
  where
    test (Token p (Id s)) = Just (IdR (Pos p) s)
    test _ = Nothing

litP :: Parser Lit
litP
  = litIntP
  <|> litStringP
  <|> litBoolP
  <|> litFloatP
  <|> litRealP
  <|> litNadaP
  where
    litIntP = tokenPrim show updatePos testInt
    testInt (Token p (LitInt x)) = Just (LInt (Pos p) x)
    testInt _ = Nothing

    litStringP = tokenPrim show updatePos testString
    testString (Token p (LitString s)) = Just (LString (Pos p) s)
    testString _ = Nothing

    litBoolP = tokenPrim show updatePos testBool
    testBool (Token p (LitBool b)) = Just (LBool (Pos p) b)
    testBool _ = Nothing

    litRealP = tokenPrim show updatePos testReal
    testReal (Token p (LitReal r)) = Just (LReal (Pos p) r)
    testReal _ = Nothing

    litFloatP = tokenPrim show updatePos testFloat
    testFloat (Token p (LitFloat f)) = Just (LFloat (Pos p) f)
    testFloat _ = Nothing

    litNadaP = do
      p <- tokenP Nada
      return (LNada p)

litIntTokenP :: Parser Int
litIntTokenP = tokenPrim show updatePos testInt
  where
    testInt (Token _ (LitInt x)) = Just x
    testInt _ = Nothing

tipoP :: Parser Tipo
tipoP = do
  tipoBase <- tipoBaseP
  tipoMatrizP tipoBase
  where
    tipoBaseP :: Parser Tipo
    tipoBaseP
      = (do
        nome@(IdR _ tipo) <- idP
        case tipo of
          "Int" -> return TInt
          "Float" -> return TFloat
          "Real" -> return TReal
          "Bool" -> return TBool
          "String" -> return TString
          "Nada" -> return TNada
          "Qualquer" -> return TQualquer
          _ -> return (TId nome))
      <|> try tipoListaP
      <|> try tipoTuplaP
      <|> try tipoDictP
      <|> (TId <$> idP)

    tipoListaP :: Parser Tipo
    tipoListaP = do
      tokenP ColEsq
      t <- tipoP
      tokenP ColDir
      return $ TList t

    tipoTuplaP :: Parser Tipo
    tipoTuplaP = do
      tokenP ParEsq
      ts <- sepBy1 tipoP (tokenP Virgula)
      tokenP ParDir
      case ts of
        [t] -> unexpected "tupla precisa de pelo menos dois tipos"
        _ -> return $ TTuple ts

    tipoDictP :: Parser Tipo
    tipoDictP = do
      tokenP ChaveEsq
      k <- tipoP
      tokenP DoisPontos
      v <- tipoP
      tokenP ChaveDir
      return $ TDict k v

    tipoMatrizP :: Tipo -> Parser Tipo
    tipoMatrizP t = option t (try $ do
      tokenP ColEsq
      r <- litIntTokenP
      tokenP Virgula
      c <- litIntTokenP
      tokenP ColDir
      tipoMatrizP (TMatrix t r c))

---------------------------------------
-- Tudo sobre Comandos ----------------
comandoP :: Parser Comando
comandoP =
  try incrementoP
  <|> try atribuicaoP
  <|> imprimaP
  <|> retorneP
  <|> inicializacaoP
  <|> declaracaoP
  <|> seP
  <|> enquantoP
  <|> chamadaCmdP
  <|> try casamentoP
    where
      incrementoP :: Parser Comando
      incrementoP = do
        id <- idP
        _ <- tokenP MaisMais
        _ <- tokenP PontoVirgula
        return (Incremento (getPos id) id)

      -- Atribuição tem a posição do token =
      atribuicaoP :: Parser Comando
      atribuicaoP = do
        lvalue <- atribuendoP
        p <- tokenP Igual
        e <- exprP
        _ <- tokenP PontoVirgula
        return (Atribuicao p lvalue e)

      atribuendoP :: Parser Atribuendo
      atribuendoP =
         try (AArray <$> idP <*> exprP)
        <|> try (do
          nome <- idP
          _ <- tokenP Ponto
          AEstrutura nome <$> idP)
        <|> try (AId <$> idP)

      -- Imprima tem a posição do IMPRIMA
      imprimaP :: Parser Comando
      imprimaP = do
        p <- tokenP Imprima
        e <- exprP
        _ <- tokenP PontoVirgula
        return (ImprimaCmd p e)

      -- Inicialização tem a posição do INICIAlIZE
      inicializacaoP :: Parser Comando
      inicializacaoP = do
        p <- tokenP Inicialize
        id <- idP
        _ <- tokenP QuatroPontos
        t <- tipoP
        _ <- tokenP Com
        e <- exprP
        _ <- tokenP PontoVirgula
        return (Inicializacao p id t e)

      -- Declaração tem a posição do DECLARE
      declaracaoP :: Parser Comando
      declaracaoP = do
        p <- tokenP Declare
        id <- idP
        _ <- tokenP QuatroPontos
        t <- tipoP
        _ <- tokenP PontoVirgula
        return (Declaracao p id t)

      -- Se tem a posição do SE
      seP :: Parser Comando
      seP = do
        p <- tokenP Se
        _ <- tokenP DoisPontos
        unc <- many unidadeCondicionalP
        _ <- tokenP FimSe
        return (SeCmd p unc)

      unidadeCondicionalP :: Parser (Expr, [Comando])
      unidadeCondicionalP = do
        cond <- exprP
        _ <- tokenP Virgula
        _ <- tokenP Faca
        _ <- tokenP DoisPontos
        cmds <- many comandoP
        _ <- tokenP FimFaca
        return (cond, cmds)

      -- Enquanto tem a posição do ENQUANTO
      enquantoP :: Parser Comando
      enquantoP = do
        p <- tokenP Enquanto
        _ <- tokenP DoisPontos
        unc <- many unidadeCondicionalP
        _ <- tokenP FimEnquanto
        return (EnquantoCmd p unc)

      -- Chamada tem a posição do identificador
      chamadaCmdP :: Parser Comando
      chamadaCmdP = do
        p <- idP
        args <- parens $ sepBy exprP (tokenP Virgula)
        tokenP PontoVirgula
        return (ChamadaCmd (getPos p) p args)

      -- Retorne tem a posição do RETORNE
      retorneP :: Parser Comando
      retorneP = do
          p <- tokenP Retorne
          e <- exprP
          _ <- tokenP PontoVirgula
          return (RetorneCmd p e)

      -- Casamento tem a posição do CASAMENTO
      casamentoP :: Parser Comando
      casamentoP = do
        p <- tokenP CasamentoTok
        noivo <- exprP
        _ <- tokenP DoisPontos
        bracos <- many bracoP
        _ <- tokenP FimCasamento
        return (CasamentoCmd p noivo bracos)

      bracoP :: Parser (Padrao, [Comando])
      bracoP = do
        variante <- idP
        captura <- optionMaybe (parens idP)
        tokenP Virgula
        tokenP Faca
        tokenP DoisPontos
        cmds <- many1 comandoP
        tokenP FimFaca
        return (Padrao variante captura, cmds)
---------------------------------------
---------------------------------------

adicionarProcedimento :: ProcedimentoR -> Programa -> Programa
adicionarProcedimento proc p = p { procedimentos = proc : procedimentos p }

adicionarFuncao :: FuncaoR -> Programa -> Programa
adicionarFuncao f p = p { funcoes = f : funcoes p}

adicionarComando :: Comando -> Programa -> Programa
adicionarComando c p = p { comandos = c : comandos p }

adicionarEnum :: EnumDecl -> Programa -> Programa
adicionarEnum e p = p { enums = e : enums p }

adicionarEstrutura :: EstruturaDecl -> Programa -> Programa
adicionarEstrutura e p = p { estruturas = e : estruturas p }

programaP :: Parser Programa
programaP
  =   (adicionarProcedimento <$> procedimentoP <*> programaP)
  <|> (adicionarFuncao   <$> try funcaoP <*> programaP)
  <|> (adicionarComando <$> comandoP <*> programaP)
  <|> (adicionarEnum <$> enumDeclP <*> programaP)
  <|> (adicionarEstrutura <$> estruturaDeclP <*> programaP)
  <|> (Programa [] [] [] [] [] <$ eof)
  where
    -- Procedimento tem a posição do token PROCEDIMENTO
    procedimentoP :: Parser ProcedimentoR
    procedimentoP = do
      p <- tokenP Procedimento
      id <- idP
      _ <- tokenP ParEsq
      parametros <- sepBy parametroP (tokenP Virgula)
      _ <- tokenP ParDir
      _ <- tokenP DoisPontos
      cmds <- many comandoP
      _ <- tokenP FimProcedimento <?> "FIM_PROCEDIMENTO."
      return (ProcedimentoR p id parametros cmds)

    -- Função tem a posição do token FUNCAO
    funcaoP :: Parser FuncaoR
    funcaoP = do
        p <- tokenP Funcao
        idFuncao <- idP
        _ <- tokenP ParEsq
        parametros <- sepBy parametroP (tokenP Virgula)
        _ <- tokenP ParDir
        _ <- tokenP Seta
        tipoRetorno <- tipoP
        _ <- tokenP DoisPontos
        cmds <- many comandoP
        _ <- tokenP FimFuncao <?> "FIM_FUNCAO."

        if not (hasRetorne cmds)
            then fail ("A funcao '" ++ show idFuncao ++ "' deve conter pelo menos um comando RETORNE.")
            else return (FuncaoR p idFuncao parametros tipoRetorno cmds)

hasRetorne :: [Comando] -> Bool
hasRetorne = any isRetorne
  where
    isRetorne (RetorneCmd _ _) = True
    isRetorne (SeCmd _ blocos) = any (\(_, cmds) -> hasRetorne cmds) blocos
    isRetorne (EnquantoCmd _ blocos) = any (\(_, cmds) -> hasRetorne cmds) blocos
    isRetorne (CasamentoCmd _ _ bracos) = any (\(_, cmds) -> hasRetorne cmds) bracos -- adicionado o casamento 
    isRetorne _ = False

parametroP :: Parser Parametro
parametroP = do
  ref <- optionMaybe (tokenP EComercial)
  id <- idP
  tokenP QuatroPontos
  t <- tipoP
  return (Parametro id t (isJust ref))

-- Chamda tem a posição do identificador
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
  indices <- many1 indiceUnitP
  return $
    foldl (EIndice (getPos v)) (EVar v) indices
  where
    indiceUnitP :: Parser Expr
    indiceUnitP = do
      _ <- tokenP ColEsq
      idx <- exprP
      _ <- tokenP ColDir
      return idx

-- Lista tem a posição do primeiro [
litListaP :: Parser Expr
litListaP = do
  p <- tokenP ColEsq
  elems <- sepBy exprP (tokenP Virgula)
  _ <- tokenP ColDir
  return (EList p elems)

-- Tupla tem a posição do primeiro (
litTuplaP :: Parser Expr
litTuplaP = do
  p <- tokenP ParEsq
  elems <- sepBy exprP (tokenP Virgula)
  _ <- tokenP ParDir
  case elems of
    [e] -> unexpected "tupla precisa de pelo menos dois elementos"
    _ -> return (ETuple p elems)

-- Dict tem a posição do primeiro {
litDictP :: Parser Expr
litDictP = do
  p <- tokenP ChaveEsq
  pairs <- sepBy pairP (tokenP Virgula)
  _ <- tokenP ChaveDir
  return (EDict p pairs)
  where
    pairP :: Parser (Expr, Expr)
    pairP = do
      key <- exprP
      tokenP DoisPontos
      value <- exprP
      return (key, value)

-- EnumDecl tem a posição do token ENUM
enumDeclP :: Parser EnumDecl
enumDeclP = do
  p <- tokenP EnumTok
  nome <- idP
  _ <- tokenP DoisPontos
  vars <- sepBy1 varianteP (tokenP Virgula)
  _ <- tokenP FimEnum
  return (EnumDecl p nome vars)
  where
    varianteP :: Parser VarianteEnum
    varianteP = do
      nome <- idP
      tipo <- optionMaybe (try (parens tipoP))
      return (VarianteEnum nome tipo)

estruturaDeclP :: Parser EstruturaDecl
estruturaDeclP = do
  p <- tokenP Estrutura
  nome <- idP
  _ <- tokenP DoisPontos
  vars <- sepBy1 varianteP (tokenP Virgula)
  _ <- tokenP FimEstrutura
  return (EstruturaDecl p nome vars)
  where
    varianteP :: Parser (Id, Tipo)
    varianteP = do
      nome <- idP
      _ <- tokenP QuatroPontos
      tipo <- tipoP
      return (nome, tipo)

-- LitEnum tem a posição do primeiro identificador
litEnumP :: Parser Expr
litEnumP = do
  enum <- idP
  _ <- tokenP QuatroPontos
  variante <- idP
  carga <- optionMaybe (parens exprP)
  return (EEnum (getPos enum) enum variante carga)

litEstruturaP :: Parser Expr
litEstruturaP = do
  p <- tokenP ChaveEsq
  campos <- sepBy1 campoP (tokenP Virgula)
  _ <- tokenP ChaveDir
  return (EEstrutura p campos)
  where
    campoP :: Parser (Id, Expr)
    campoP = do
      campo <- idP
      _ <- tokenP Igual
      valor <- exprP
      return (campo, valor)

litAcessoP :: Parser Expr
litAcessoP = do
  nome <- idP
  _ <- tokenP Ponto
  EAcesso (getPos nome) nome <$> idP

-- litMatrizP :: Parser Expr
-- litMatrizP = do
--   start <- tokenP ColEsq
--   rows <- sepBy (sepBy exprP (tokenP Virgula)) (tokenP PontoVirgula)
--   end <- tokenP ColDir
--   return (EMatrix (Pos start end) rows)

-- Documentação do buildExpressionParser:
-- https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Expr.html
exprP :: Parser Expr
exprP = buildExpressionParser table term <?> "Expression"
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
      p <- tokenP tk
      pure $ \l r ->
        EOpBin p op l r

    unOpP tk op = do
      p <- tokenP tk
      pure $ \e -> EOpUn p op e

    convP :: Parser Expr
    convP =
      choice
        [ try $ convP' op tok
        | (op, tok) <-
            [ (Conv TInt, Int)
            , (Conv TReal, Real)
            , (Conv TBool, Bool)
            , (Conv TString, String)
            , (Conv TFloat, Float)
            ]
        ]

    convP' op tok = do
      p <- tokenP tok
      e <- parens exprP
      return $ EOpUn p op e

    term =
        try chamadaP
        <|> try indiceP
        <|> try litListaP
        <|> try litTuplaP
        <|> try litDictP
        <|> try litEnumP
        <|> try litAcessoP
        <|> try litEstruturaP
        -- <|> try litMatrizP
        <|> try (ELit <$> litP)
        <|> EVar <$> idP
        <|> convP
        <|> (ELeia <$> tokenP Leia)
        <|> parens exprP
