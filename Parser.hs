module Parser where

import Lexer
import Repr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Pos (newPos)
import Data.Maybe (isJust)
import Data.Char (isDigit)

type Parser a = Parsec [Token] () a

---------------------------------------
-- Helpers ----------------------------
parens :: Parser a -> Parser a
parens p = tokenP ParEsq *> p <* tokenP ParDir

updatePos :: SourcePos -> Token -> s -> SourcePos
updatePos sp (Token (AlexPn _ line col) _) _ =
  newPos (sourceName sp) line col

-- Joga fora o Token
tokenP :: TokenKind -> Parser AlexPosn
tokenP k = tokenPrim show updatePos test
   where
    test t@(Token p k')
      | k == k'   = Just p
      | otherwise = Nothing
---------------------------------------
---------------------------------------

idP :: Parser Id
idP = tokenPrim show updatePos test
  where
    test (Token p (Id s)) = Just (IdR (Pos p p) s)
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
    testInt (Token p (LitInt x)) = Just (LInt (Pos p p) x)
    testInt _ = Nothing

    litStringP = tokenPrim show updatePos testString
    testString (Token p (LitString s)) = Just (LString (Pos p p) s)
    testString _ = Nothing

    litBoolP = tokenPrim show updatePos testBool
    testBool (Token p (LitBool b)) = Just (LBool (Pos p p) b)
    testBool _ = Nothing

    litRealP = tokenPrim show updatePos testReal
    testReal (Token p (LitReal r)) = Just (LReal (Pos p p) r)
    testReal _ = Nothing

    litFloatP = tokenPrim show updatePos testFloat
    testFloat (Token p (LitFloat f)) = Just (LFloat (Pos p p) f)
    testFloat _ = Nothing

    litNadaP = do
      p <- tokenP Nada
      return (LNada (Pos p p))

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
        IdR _ tipo <- idP
        case tipo of
          "Int" -> return TInt
          "Float" -> return TFloat
          "Real" -> return TReal
          "Bool" -> return TBool
          "String" -> return TString)
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
  
      retorneP :: Parser Comando
      retorneP = do
          start <- tokenP Retorne
          e <- exprP
          end <- tokenP PontoVirgula
          return (RetorneCmd (Pos start end) e)

      casamentoP :: Parser Comando
      casamentoP = do
        start <- tokenP CasamentoTok
        noivo <- exprP
        tokenP DoisPontos
        bracos <- many bracoP
        end <- tokenP FimCasamento
        return (CasamentoCmd (Pos start end) noivo bracos)

      bracoP :: Parser (Padrao, [Comando])
      bracoP = do
        variante <- idP
        captura <- optionMaybe (parens idP)
        tokenP Seta
        cmds <- many1 comandoP
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

programaP :: Parser Programa
programaP
  =   (adicionarProcedimento <$> procedimentoP <*> programaP)
  <|> (adicionarFuncao   <$> try funcaoP <*> programaP)
  <|> (adicionarComando <$> comandoP <*> programaP) 
  <|> (adicionarEnum <$> enumDeclP <*> programaP)
  <|> (Programa [] [] [] [] <$ eof)
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

    funcaoP :: Parser FuncaoR
    funcaoP = do
        start <- tokenP Funcao
        idFuncao <- idP
        tokenP ParEsq
        parametros <- sepBy parametroP (tokenP Virgula)
        tokenP ParDir
        tokenP Seta
        tipoRetorno <- tipoP
        tokenP DoisPontos
        cmds <- many comandoP
        end <- tokenP FimFuncao <?> "FIM_FUNCAO."
        
        if not (hasRetorne cmds)
            then fail ("A funcao '" ++ show idFuncao ++ "' deve conter pelo menos um comando RETORNE.")
            else return (FuncaoR (Pos start end) idFuncao parametros tipoRetorno cmds)

hasRetorne :: [Comando] -> Bool
hasRetorne = any isRetorne
  where
    isRetorne (RetorneCmd _ _) = True
    isRetorne (SeCmd _ blocos) = any (\(_, cmds) -> hasRetorne cmds) blocos
    isRetorne (EnquantoCmd _ blocos) = any (\(_, cmds) -> hasRetorne cmds) blocos
    isRetorne _ = False

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

litListaP :: Parser Expr
litListaP = do
  start <- tokenP ColEsq
  elems <- sepBy exprP (tokenP Virgula)
  end <- tokenP ColDir
  return (EList (Pos start end) elems)

litTuplaP :: Parser Expr
litTuplaP = do
  start <- tokenP ParEsq
  elems <- sepBy exprP (tokenP Virgula)
  end <- tokenP ParDir
  case elems of
    [e] -> unexpected "tupla precisa de pelo menos dois elementos"
    _ -> return (ETuple (Pos start end) elems)

litDictP :: Parser Expr
litDictP = do
  start <- tokenP ChaveEsq
  pairs <- sepBy pairP (tokenP Virgula)
  end <- tokenP ChaveDir
  return (EDict (Pos start end) pairs)
  where
    pairP :: Parser (Expr, Expr)
    pairP = do
      key <- exprP
      tokenP DoisPontos
      value <- exprP
      return (key, value)

enumDeclP :: Parser EnumDecl
enumDeclP = do
  start <- tokenP EnumTok
  nome <- idP
  vars <- sepBy1 varianteP (tokenP Virgula)
  end <- tokenP FimEnum
  return (EnumDecl (Pos start end) nome vars)
  where
    varianteP :: Parser VarianteEnum
    varianteP = do
      nome <- idP
      tipo <- optionMaybe (try (parens tipoP))
      return (VarianteEnum nome tipo)

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
            [ (Conv TInt, Int)
            , (Conv TReal, Real)
            , (Conv TBool, Bool)
            , (Conv TString, String)
            , (Conv TFloat, Float)
            ]
        ]

    convP' op tok = do
      start <- tokenP tok
      e <- parens exprP
      return $ EOpUn (getPos e) op e

    term =
        try chamadaP
        <|> try indiceP
        <|> try litListaP
        <|> try litTuplaP
        <|> try litDictP
        -- <|> try litMatrizP
        <|> try (ELit <$> litP)
        <|> EVar <$> idP
        <|> convP
        <|> (do p <- tokenP Leia; return (ELeia (Pos p p)))
        <|> parens exprP
