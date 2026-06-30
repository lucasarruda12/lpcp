module Analyser (analiseEstatica) where

import Repr
import Control.Monad
import Data.Functor
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------------
-- O que está sendo checado até então -
-- Em comandos:
-- -- Se as variáveis já foram declaradas
-- -- O ltipo e o rtipo de um atribuicao
-- Em expressoes:
-- -- Erros de tipo
-- -- Se as variaveis foram declaradas
---------------------------------------
---------------------------------------

---------------------------------------
-- Helpers ---------------------------- 
(+-+) :: String -> String -> String
s1 +-+ s2 = s1 ++ ' ':s2

error' :: String -> a
error' s = error ("[No analisador de semântica estática] " ++ s)
---------------------------------------
---------------------------------------

data ErroSemanticaEstatica
  = ErroDeTipo Tipo Tipo
  | ErroDeTipoAttr (String, Tipo) (String, Tipo)
  | NumeroIncorretoDeParametros Int Int
  | JaDeclarado String Pos
  | NaoDeclarado String
  | NaoDefinido Tipo
  | JaDefinido String Pos
  | TipoNaoIndexavel Tipo
  | CampoNaoExiste Tipo Id
  | Contexto String ErroSemanticaEstatica

instance Show ErroSemanticaEstatica where
  show (ErroDeTipo t1 t2) = "Esperava" +-+ show t1 +-+ "e recebi" +-+ show t2
  show (ErroDeTipoAttr (n1, t1) (n2, t2)) = "A atribuição de" +-+ n1 +-+ "esperava" +-+ show t1 +-+ "mas recebeu" +-+ n2 +-+ "::" +-+ show t2
  show (NumeroIncorretoDeParametros n1 n2) = "Numero Incorreto de Parâmtros. Esperava" +-+ show n1 +-+ "e recebi" +-+ show n2
  show (JaDeclarado nome pos) = "A variável" +-+ nome +-+ "já foi declarada em" +-+ show pos
  show (NaoDeclarado nome) = "Não declarado:" +-+ nome
  show (NaoDefinido t) = "O tipo" +-+ show t +-+ "não foi definido"
  show (JaDefinido nome pos) = nome +-+ "já foi definido em" +-+ show pos
  show (Contexto c err) = c ++ "\n>" +-+ show err
  show (TipoNaoIndexavel t) = "O tipo" +-+ show t +-+ "não é indexável"
  show (CampoNaoExiste t ident) = "O campo" +-+ show ident +-+ "não existe em" +-+ show t

primitivo :: Tipo -> Bool
primitivo = 
  (`Set.member` Set.fromList [TInt, TFloat, TReal, TString, TBool, TNada, TQualquer])

numerico :: Tipo -> Bool
numerico = 
  (`Set.member` Set.fromList [TInt, TFloat, TReal])

data TabelaDeSimbolos = TS
  { ps :: Map.Map String (Pos, [Tipo])
  , fs :: Map.Map String (Pos, [Tipo], Tipo)
  , variaveis :: [Map.Map String (Pos, Tipo)]
  -- Essa parte ficou bastante cheia de informacao:
  , ens :: Map.Map Tipo (Pos, [(Id, Tipo)])
  , es :: Map.Map Tipo (Pos, [(Id, Tipo)])
  }

tabelaVazia :: TabelaDeSimbolos
tabelaVazia = TS
  { ps = Map.empty
  , fs = Map.empty
  , variaveis = [Map.empty]
  , ens = Map.empty
  , es = Map.empty
  -- , tipos = Map.fromList $ (, (Nothing, [])) <$> 
  }

tipos :: TabelaDeSimbolos -> Map.Map Tipo (Maybe Pos)
tipos ts = Map.unions [declEs, declEns, primitivos]
  where
    declEs = Map.map (\(p, _) -> Just p) (es ts)
    declEns = Map.map (\(p, _) -> Just p) (ens ts)
    primitivos = Map.fromList $ (, Nothing) <$> [TInt, TFloat, TReal, TString, TBool, TNada, TQualquer]

analiseEstatica :: Programa -> Maybe String
analiseEstatica p = case runState (runExceptT (cheque p)) tabelaVazia of
  (Left err, _) -> Just (show err)
  (Right _, _) -> Nothing

type CheqM a = ExceptT ErroSemanticaEstatica (State TabelaDeSimbolos) a

-- É importante que seja nessa ordem: posso declarar variáveis no escopo externo que devem ser acessíveis no escopo interno dos procedimentos.
-- Checar o escopo externo inclui adicionar as variáveis deles na tabela de símbolos. Isso tem que acontecer antes de checar os procedimentos.
cheque :: Programa -> CheqM ()
cheque (Programa ps fs ens es cmds) = do
  mapM_ addEnum ens
  mapM_ addEstrutura es
  mapM_ addProcedimento ps -- Adiciona todos os procedimentos na tabela de símbolos
  mapM_ addFuncao fs -- Adiciona as funções
  mapM_ chequeComando cmds -- Checa o escopo externo
  mapM_ chequeProcedimento ps -- Checa os procedimentos
  mapM_ chequeFuncao fs -- Checa as funções

addFuncao :: FuncaoR -> CheqM ()
addFuncao f@(FuncaoR pos (IdR _ nome) pars tipo _)
  = comContexto f $ do
    definidas <- gets fs
    case Map.lookup nome definidas of
      Just (p', _, _) -> throwError (JaDefinido nome p')
      Nothing -> pure ()
    pars' <- mapM chequeParametro pars
    _ <- chequeTipo tipo
    modify $ \ts
      -> ts { fs = Map.insert nome (pos, pars', tipo) definidas }

addProcedimento :: ProcedimentoR -> CheqM ()
addProcedimento p@(ProcedimentoR pos (IdR _ nome) pars _)
  = comContexto p $ do
    definidos <- gets ps
    case Map.lookup nome definidos of
      Just (p', _) -> throwError (JaDefinido nome p')
      Nothing -> pure()
    pars' <- mapM chequeParametro pars
    modify $ \ts
      -> ts { ps = Map.insert nome (pos, pars') definidos }

addEnum :: EnumDecl -> CheqM ()
addEnum (EnumDecl p ident variantes) = do
  let (IdR _ nome) = ident
  let novoTipo = TId ident
  let variantes' = map maybeToNada variantes
  definidos <- gets tipos
  case Map.lookup novoTipo definidos of
    Just (Just p') -> throwError (JaDefinido nome p')
    Just Nothing 
      -> error' "Encontrei um tipo nomeado com o mesmo nome de um tipo primitivo. O lexer não deveria deixar isso acontecer."
    Nothing -> pure ()
  modify $ \ts
    -> ts { ens = Map.insert novoTipo (p, variantes') (ens ts) }

  where
    maybeToNada :: VarianteEnum -> (Id, Tipo)
    maybeToNada (VarianteEnum i mt) = 
      (i, fromMaybe TNada mt)

addEstrutura :: EstruturaDecl -> CheqM ()
addEstrutura (EstruturaDecl p ident campos) = do
  let (IdR _ nome) = ident
  let novoTipo = TId ident
  definidos <- gets tipos
  case Map.lookup novoTipo definidos of
    Just (Just p') -> throwError (JaDefinido nome p')
    Just Nothing 
      -> error' "Encontrei um tipo nomeado com o mesmo nome de um tipo primitivo. O lexer não deveria deixar isso acontecer."
    Nothing -> pure ()
  modify $ \ts
    -> ts { es = Map.insert novoTipo (p, campos) (es ts) }
  
chequeTipo :: Tipo -> CheqM ()
chequeTipo tipo = do
  case tipo of
    TList interno -> chequeTipo interno
    TTuple internos -> mapM_ chequeTipo internos
    TDict chave valor -> mapM_ chequeTipo [chave, valor]
    TMatrix interno _ _ -> chequeTipo interno
    _ -> do
      definidos <- gets tipos
      unless (Map.member tipo definidos)
        (throwError (NaoDefinido tipo))

chequeTipos :: Tipo -> Tipo -> CheqM Tipo
chequeTipos t1 t2 = do
  chequeTipo t1 -- checo se os dois existem
  chequeTipo t2
  case (t1, t2) of
    (TQualquer, _)  -> pure t2
    (_, TQualquer)  -> pure t1
    _ | t1 == t2    -> pure t1
    _ | otherwise   -> throwError (ErroDeTipo t1 t2)

chequeParametro :: Parametro -> CheqM Tipo
chequeParametro (Parametro _ tipo _)
  = chequeTipo tipo $> tipo

declParametro :: Parametro -> CheqM ()
declParametro (Parametro (IdR p nome) tipo _)
  = declVar nome p tipo

chequeProcedimento :: ProcedimentoR -> CheqM ()
chequeProcedimento p@(ProcedimentoR _ _ pars cmds)
  = comEscopo (comContexto p
    (mapM_ declParametro pars >> mapM_ chequeComando cmds))

chequeFuncao :: FuncaoR -> CheqM ()
chequeFuncao f@(FuncaoR _ _ pars _ cmds)
  = comEscopo (comContexto f
    (mapM_ declParametro pars >> mapM_ chequeComando cmds))

-- Usa esse aqui como exemplo:
chequeComando :: Comando -> CheqM ()
chequeComando cmd =
  comContexto cmd $ case cmd of -- <- comContexto
    (ImprimaCmd _ e) ->
      void (chequeExpr e)

    -- Usa esse aqui como exemplo:
    (Inicializacao p (IdR _ nome) ltipo e) -> do
      rtipo <- chequeExpr e -- <- encontre o tipo de e
      case ltipo of
        TMatrix t _ _ -> 
          if rtipo /= TList (TList t) 
            then throwError
              (ErroDeTipoAttr (nome, t) (show e, rtipo))
            else declVar nome p ltipo
        _ -> 
          if ltipo /= rtipo -- <- o tipo de e é igual ao tipo da variável que eu to inicializando?
            then throwError -- Se não for: erro de tipo.a
              (ErroDeTipoAttr (nome, ltipo) (show e, rtipo))
            else declVar nome p ltipo -- Se for, adiciona a variável com esse nome e esse tipo lá na tabela de símbolos

    (Declaracao p (IdR _ nome) tipo) ->
      declVar nome p tipo

    (Atribuicao _ atribuendo rvalue) ->
      chequeAtribuicao atribuendo rvalue

    -- Unidade de Comandos de Condicionais (UNC) :D
    (EnquantoCmd _ uncs) -> comEscopo
      (mapM_ chequeUnc uncs)

    (SeCmd _ uncs) -> comEscopo
      (mapM_ chequeUnc uncs)

    (Incremento _ (IdR _ nome)) ->
      void (getVar nome)

    (RetorneCmd _ e) ->
      void (chequeExpr e)

    (ChamadaCmd _ (IdR _ nome) attrs) ->
      chequeChamadaCmd nome attrs

    (CasamentoCmd _ noivo bracos) -> do
      noivoTipo <- chequeExpr noivo 
      case noivoTipo of
        (TId _) -> mapM_ (chequeBraco noivoTipo) bracos
        _ -> throwError (TipoNaoIndexavel noivoTipo)

chequeBraco :: Tipo -> (Padrao, [Comando]) -> CheqM ()
chequeBraco t (Padrao variante mcaptura, cmds) = do
  definidos <- gets es
  case Map.lookup t definidos of
    Just (_, variantes) ->
      case lookup variante variantes of
        Just t2 -> comEscopo $ do
          case mcaptura of
            Just (IdR p captura) -> declVar captura p t2
            Nothing -> pure ()
          mapM_ chequeComando cmds
        Nothing -> 
          throwError (CampoNaoExiste t variante)
    Nothing -> throwError (TipoNaoIndexavel t)

chequeChamadaCmd :: String -> [Expr] -> CheqM ()
chequeChamadaCmd nome attrs = do
  (_, pars) <- getProcedimento nome
  attrs' <- mapM chequeExpr attrs
  let (l1, l2) = (length pars, length attrs')
  unless (l1 == l2)
    (throwError (NumeroIncorretoDeParametros l1 l2))
  mapM_ (uncurry chequeTipos) (zip pars attrs')

chequeAtribuicao :: Atribuendo -> Expr -> CheqM ()
chequeAtribuicao atribuendo rvalue = do
  rtipo <- chequeExpr rvalue
  case atribuendo of
    (AId (IdR _ nome)) -> void $
      getVar nome >>= (`chequeTipos` rtipo)

    (AArray (IdR _ nome) idx) -> do
      ltipo <- getVar nome
      case ltipo of
        (TList t) -> void $
          chequeExpr idx >>= chequeTipos TInt
          >> chequeTipos t rtipo
        (TTuple _) -> void $
          chequeExpr idx >>= chequeTipos TInt
        (TDict ct vt) -> void $
          chequeExpr idx >>= chequeTipos ct
          >> chequeTipos vt rtipo
        _ -> throwError (TipoNaoIndexavel ltipo)

    -- Esse tá difícil de entender, mas com calma vai!
    (AEstrutura (IdR _ nome) campo) -> do
      ltipo <- getVar nome
      estruturas <- gets es
      case ltipo of
        (TId _) ->
          case Map.lookup ltipo estruturas of
            Nothing -> throwError (TipoNaoIndexavel ltipo)

            Just (_, campos) -> 
              case lookup campo campos of
                Just campoTipo -> void $ 
                  chequeTipos campoTipo rtipo

                Nothing -> throwError (CampoNaoExiste ltipo campo)
        _ -> 
          throwError (TipoNaoIndexavel ltipo)

chequeUnc :: (Expr, [Comando]) -> CheqM ()
chequeUnc (e, cmds) = do
  t1 <- chequeExpr e
  unless (t1 == TBool)
    (comContexto e (throwError $ ErroDeTipo TBool t1))
  mapM_ chequeComando cmds

-- A parte de expressões tá complicada. Boa sorte!
chequeExpr :: Expr -> CheqM Tipo
chequeExpr e = comContexto'
  (show (getPos e) +-+ "Na expressão" +-+ show e)
    (chequeExpr' e)

chequeExpr' :: Expr -> CheqM Tipo
chequeExpr' (ELit l) = case l of
  LInt _ _ -> pure TInt
  LString _ _ -> pure TString
  LBool _ _ -> pure TBool
  LFloat _ _ -> pure TFloat
  LReal _ _ -> pure TReal
  LNada _  -> pure TNada

chequeExpr' (EList _ []) = pure (TList TQualquer)
chequeExpr' (EList _ [e]) = TList <$> chequeExpr' e
chequeExpr' (EList p (e:es)) = do
  t1 <- chequeExpr' e
  tlist <- chequeExpr' (EList p es)
  _ <- case tlist of
    TList t2 -> chequeTipos t2 t1
    _ -> 
      error' "chequeExpr' (EList) retornou algo diferente de TList"
  return (TList t1)

chequeExpr' (EDict _ []) = pure (TDict TQualquer TQualquer)
chequeExpr' (EDict _ [(c, v)]) 
  = TDict <$> chequeExpr' c <*> chequeExpr' v
chequeExpr' (EDict p ((c, v):cvs)) = do
  tc <- chequeExpr' c
  tv <- chequeExpr' v
  tdict <- chequeExpr' (EDict p cvs)
  case tdict of
    TDict tc' tv' -> 
      chequeTipos tc' tc
      >> chequeTipos tv' tv
    _ ->
      error' "chequeExpr' (EDict) retornou algo diferente de TDict"

-- chequeExpr' (EIndice _ 

chequeExpr' fe@(EOpUn _ op e) = do
  t <- chequeExpr' e
  comContexto fe
    (chequeUnOp op t)

chequeExpr' fe@(EOpBin _ op e1 e2) = do
  t1 <- chequeExpr' e1
  t2 <- chequeExpr' e2
  comContexto fe
   (chequeOpBin op t1 t2)

chequeExpr' (ELeia _) = pure TString
chequeExpr' (EVar (IdR _ nome)) = getVar nome

chequeExpr' (EChamada _ (IdR _ nome) attrs) = do
  (_, pars, tipo) <- getFuncao nome
  attrs' <- mapM chequeExpr attrs
  let (l1, l2) = (length pars, length attrs')
  unless (l1 == l2)
    (throwError (NumeroIncorretoDeParametros l1 l2))
  mapM_ (uncurry chequeTipos) (zip pars attrs')
  return tipo

chequeUnOp :: OpUn -> Tipo -> CheqM Tipo
chequeUnOp op t = do
  case op of
    Neg ->
      if numerico t
      then return t
      else throwError (ErroDeTipo TInt t)
    NaoOp -> 
      chequeTipos TBool t
    Conv t2 ->
      if primitivo t
      then return t2
      else throwError (ErroDeTipo t2 t)

chequeOpBin :: OpBin -> Tipo -> Tipo -> CheqM Tipo
chequeOpBin op t1 t2 = do
  let tipos_nums = all numerico [t1, t2]
  let op_numerica = op `elem` [Soma, Mul, Div, Exp, Mod, Sub]
  let op_comp = op `elem` [Menor, Maior, MenorIgualOp, MaiorIgualOp, IgualOp, DiferenteOp]
  let bool_op = op `elem` [AndOp, OrOp]
  case () of
    () | op_numerica && tipos_nums -> 
      chequeTipos t1 t2

    () | op_numerica && numerico t1 ->
      throwError (ErroDeTipo t1 t2)

    () | op_numerica && numerico t2 ->
      throwError (ErroDeTipo t2 t1)

    () | op_comp -> 
      chequeTipos t1 t2 >> pure TBool

    () | bool_op -> 
      chequeTipos TBool t1 >> chequeTipos TBool t2

    () | otherwise ->
      error' "Na checagem de tipos: Tipo não identificado"

comEscopo :: CheqM a -> CheqM a
comEscopo acao = do
  modify $ \ts
    -> ts { variaveis = Map.empty : variaveis ts }
  a <- acao
  modify $ \ts
    -> ts { variaveis = drop 1 (variaveis ts) }
  return a

-- comCOntexto: Em alguns momentos eu quero uma coisa simples. Em outros uma coisa mais complicada:

-- Esse constroi a mensagem por você
comContexto :: (Show a, Positional a) => a -> CheqM b -> CheqM b
comContexto r = comContexto'
  (show (getPos r) +-+ show r)

-- Esse pede a mensagem
comContexto' :: String -> CheqM b -> CheqM b
comContexto' s c =
  catchError c (throwError . Contexto s)

declVar :: String -> Pos -> Tipo -> CheqM ()
declVar nome pos tipo = do
  vars <- gets variaveis
  case vars of
    [] -> error "Algo deu muito errado! Tentei adicionar uma variável e pilha da tabela de símbolos está completamente vazia"
    (v:vs) -> do
      case Map.lookup nome v of
        Just (pos', _) -> throwError (JaDeclarado nome pos')
        Nothing -> modify $ \ts -> ts { variaveis = Map.insert nome (pos, tipo) v : vs }

getVar :: String -> CheqM Tipo
getVar nome = do
  vars <- gets variaveis
  getVar' nome vars
  where
    getVar' nome' (v:vs) = case Map.lookup nome' v of
      Just (_, tipo) -> pure tipo
      Nothing -> getVar' nome vs
    getVar' nome' [] = throwError (NaoDeclarado nome')

getFuncao :: String -> CheqM (Pos, [Tipo], Tipo)
getFuncao nome = do
  definidos <- gets fs
  case Map.lookup nome definidos of
    Just (p, pars, tipo) -> return (p, pars, tipo)
    Nothing -> throwError (NaoDeclarado nome)

getProcedimento :: String -> CheqM (Pos, [Tipo])
getProcedimento nome = do
  definidos <- gets ps
  case Map.lookup nome definidos of
    Just (p, ts) -> return (p, ts)
    Nothing -> throwError (NaoDeclarado nome)
