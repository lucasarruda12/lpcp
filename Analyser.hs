module Analyser (analiseEstatica) where

import Repr
import Control.Monad
import Data.Functor
import Control.Monad.Except
import Control.Monad.State
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


primitivos :: Set.Set Tipo
primitivos = Set.fromList [TInt, TFloat, TReal, TString, TBool, TNada]

numericos :: Set.Set Tipo
numericos = Set.fromList [TInt, TFloat, TReal]

data TabelaDeSimbolos = TS 
  { ps :: Map.Map String (Pos, [Tipo])
  , fs :: Map.Map String (Pos, [Tipo], Tipo)
  , variaveis :: [Map.Map String (Pos, Tipo)]
  , tipos :: Set.Set Tipo
  }

tabelaVazia :: TabelaDeSimbolos 
tabelaVazia = TS Map.empty Map.empty [Map.empty] primitivos

analiseEstatica :: Programa -> Maybe String
analiseEstatica p = case runState (runExceptT (cheque p)) tabelaVazia of
  (Left err, _) -> Just (show err)
  (Right _, _) -> Nothing

type CheqM a = ExceptT ErroSemanticaEstatica (State TabelaDeSimbolos) a

-- É importante que seja nessa ordem: posso declarar variáveis no escopo externo que devem ser acessíveis no escopo interno dos procedimentos.
-- Checar o escopo externo inclui adicionar as variáveis deles na tabela de símbolos. Isso tem que acontecer antes de checar os procedimentos.
cheque :: Programa -> CheqM ()
cheque (Programa ps fs ens es cmds) = do
  mapM_ addProcedimento ps -- Adiciona todos os procedimentos na tabela de símbolos
  mapM_ addFuncao fs -- Adiciona as funções?
  mapM_ chequeComando cmds -- Checa o escopo externo
  mapM_ chequeProcedimento ps -- Checa os procedimentos
  mapM_ chequeFuncao fs -- Checa as funções?

addFuncao :: FuncaoR -> CheqM ()
addFuncao f@(FuncaoR pos (IdR _ nome) pars tipo _) 
  = comContexto f $ do
    declaradas <- gets fs
    when (Map.member nome declaradas)
      (throwError (JaDefinido nome pos))
    pars' <- mapM chequeParametro pars 
    _ <- chequeTipo tipo
    modify $ \ts 
      -> ts { fs = Map.insert nome (pos, pars', tipo) declaradas }

addProcedimento :: ProcedimentoR -> CheqM ()
addProcedimento p@(ProcedimentoR pos (IdR _ nome) pars _)
  = comContexto p $ do
    declarados <- gets ps
    when (Map.member nome declarados)
      (throwError (JaDefinido nome pos))
    pars' <- mapM chequeParametro pars
    modify $ \ts
      -> ts { ps = Map.insert nome (pos, pars') declarados }

chequeTipo :: Tipo -> CheqM ()
chequeTipo tipo = do
  definidos <- gets tipos
  unless (Set.member tipo definidos)
    (throwError (NaoDefinido tipo))

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
      rtipo <- chequeExpr e -- <- encntre o tipo de e
      if ltipo /= rtipo -- <- o tipo de e é igual ao tipo da variável que eu to inicializando?
      then throwError -- Se não for: erro de tipo.
        (ErroDeTipoAttr (nome, ltipo) (show e, rtipo))
      else declVar nome p ltipo -- Se for, adiciona a variável com esse nome e esse tipo lá na tabela de símbolos

    (Declaracao p (IdR _ nome) tipo) ->
      declVar nome p tipo

    (Atribuicao _ (AId (IdR _ lnome)) rvalue) ->
      chequeAtribuicao lnome rvalue

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
      void (chequeChamadaCmd nome attrs)

chequeChamadaCmd :: String -> [Expr] -> CheqM Tipo
chequeChamadaCmd nome attrs = do
  (_, pars) <- getProcedimento nome
  attrs' <- mapM chequeExpr attrs
  let (l1, l2) = (length pars, length attrs')
  unless (l1 == l2)
    (throwError (NumeroIncorretoDeParametros l1 l2))
  mapM_ chequeTipos (zip pars attrs')
  return TNada
  where 
    chequeTipos :: (Tipo, Tipo) -> CheqM ()
    chequeTipos (t1, t2) = 
      unless (t1 == t2)
        (throwError (ErroDeTipo t1 t2))

chequeAtribuicao :: String -> Expr -> CheqM ()
chequeAtribuicao lnome rvalue = do
  ltipo <- getVar lnome
  rtipo <- chequeExpr rvalue
  unless (ltipo == rtipo)
    (throwError $ ErroDeTipo ltipo rtipo)

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

chequeExpr' (EChamada p (IdR _ nome) attrs) = do
  (pos, pars, tipo) <- getFuncao nome
  attrs' <- mapM chequeExpr attrs
  let (l1, l2) = (length pars, length attrs')
  unless (l1 == l2)
    (throwError (NumeroIncorretoDeParametros l1 l2))
  mapM_ chequeTipos (zip pars attrs')
  return tipo
  where 
    chequeTipos :: (Tipo, Tipo) -> CheqM ()
    chequeTipos (t1, t2) = 
      unless (t1 == t2)
        (throwError (ErroDeTipo t1 t2))

chequeUnOp :: OpUn -> Tipo -> CheqM Tipo
chequeUnOp op t = do
  case op of
    Neg -> 
      if t `Set.member` numericos 
      then return t 
      else throwError (ErroDeTipo TInt t)
    NaoOp ->
      if t == TBool 
      then return TBool
      else throwError (ErroDeTipo TBool t)
    Conv t2 ->
      if t `Set.member` primitivos
      then return t2
      else throwError (ErroDeTipo t2 t)

chequeOpBin :: OpBin -> Tipo -> Tipo -> CheqM Tipo
chequeOpBin op t1 t2 = do
  let tipos_nums = all (`Set.member` numericos) [t1, t2]
  let tipos_iguais = t1 == t2
  let op_numerica = op `elem` [Soma, Mul, Div, Exp, Mod, Sub]
  let op_comp = op `elem` [Menor, Maior, MenorIgualOp, MaiorIgualOp, IgualOp, DiferenteOp]
  let bool_op = op `elem` [AndOp, OrOp]
  case () of
    () | op_numerica && tipos_nums ->
      if tipos_iguais
        then pure t1
        else throwError (ErroDeTipo t1 t2)
    () | op_numerica && Set.member t1 numericos ->
      throwError (ErroDeTipo t1 t2)
    () | op_numerica && Set.member t2 numericos ->
      throwError (ErroDeTipo t2 t1)
    () | op_comp ->
      if tipos_iguais
        then pure TBool
        else throwError (ErroDeTipo t1 t2)
    () | bool_op ->
      if t1 == TBool && t2 == TBool
        then pure TBool
        else throwError (ErroDeTipo TBool (if t1 /= TBool then t1 else t2))
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
    Just (p, tipos, tipo) -> return (p, tipos, tipo)
    Nothing -> throwError (NaoDeclarado nome)

getProcedimento :: String -> CheqM (Pos, [Tipo])
getProcedimento nome = do
  definidos <- gets ps
  case Map.lookup nome definidos of
    Just (p, tipos) -> return (p, tipos)
    Nothing -> throwError (NaoDeclarado nome)
