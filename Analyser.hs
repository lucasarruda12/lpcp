module Analyser (analiseEstatica) where

import Repr
import Control.Monad
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
---------------------------------------
---------------------------------------

data ErroSemanticaEstatica
  = ErroDeTipo Tipo Tipo
  | ErroDeTipoAttr (String, Tipo) (String, Tipo)
  | NumeroIncorretoDeParametros
  | JaDeclarado String Pos
  | NaoDeclarado String
  | Contexto String ErroSemanticaEstatica
  | Outro String

instance Show ErroSemanticaEstatica where
  show (ErroDeTipoAttr (n1, t1) (n2, t2)) = "A atribuição de" +-+ n1 +-+ "esperava" +-+ show t1 +-+ "mas recebeu" +-+ n2 +-+ "::" +-+ show t2
  show (ErroDeTipo t1 t2) = "Esperava" +-+ show t1 +-+ "e recebi" +-+ show t2
  show (Contexto c err) = c ++ "\n>" +-+ show err
  show (NaoDeclarado nome) = "Variável não declarada:" +-+ nome

primitivos :: Set.Set Tipo
primitivos = Set.fromList [TInt, TFloat, TReal, TString, TBool]

numericos :: Set.Set Tipo
numericos = Set.fromList [TInt, TFloat, TReal]

data TabelaDeSimbolos = TS 
  { ps :: Map.Map String Pos
  , funcoes :: Map.Map String Pos
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

cheque :: Programa -> CheqM ()
cheque (Programa ps cmds) = do
  mapM_ addProcedimento ps
  mapM_ chequeComando cmds

addProcedimento :: ProcedimentoR -> CheqM ()
addProcedimento = undefined

chequeComando :: Comando -> CheqM ()
chequeComando (ImprimaCmd p e) = void $ comContexto (show p) (chequeExpr e)
chequeComando (Inicializacao p (IdR _ nome) ltipo e) = do
  rtipo <- chequeExpr e
  if ltipo /= rtipo
  then comContexto (show p) $
    throwError (ErroDeTipoAttr (nome, ltipo) (show e, rtipo))
  else declVar nome p ltipo
chequeComando (Declaracao p (IdR _ nome) tipo) =
  comContexto (show p) (declVar nome p tipo)
chequeComando (Atribuicao p (AId (IdR _ lnome)) rvalue) =
  comContexto (show p) (chequeAtribuicao lnome rvalue)
chequeComando (EnquantoCmd p uncs) =
  comContexto (show p) (mapM_ chequeUnc uncs)
chequeComando (SeCmd p uncs) =
  comContexto (show p) (mapM_ chequeUnc uncs)

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
    (comContexto (show . getPos $ e) (throwError $ ErroDeTipo TBool t1))
  mapM_ chequeComando cmds



chequeExpr :: Expr -> CheqM Tipo
chequeExpr e = comContexto (show (getPos e) +-+ "Na expressão" +-+ show e) (chequeExpr' e)

chequeExpr' :: Expr -> CheqM Tipo
chequeExpr' (ELit l) = case l of
  LInt _ _ -> pure TInt
  LString _ _ -> pure TString
  LBool _ _ -> pure TBool
  LFloat _ _ -> pure TFloat
  LReal _ _ -> pure TReal
chequeExpr' fe@(EOpUn p op e) = do
  t <- chequeExpr' e
  chequeUnOp (show p +-+ show fe +-+ show p) op t
chequeExpr' fe@(EOpBin p op e1 e2) = do
  t1 <- chequeExpr' e1
  t2 <- chequeExpr' e2
  chequeOpBin (show p +-+ show fe) op t1 t2
chequeExpr' (ELeia _) = pure TString
chequeExpr' (EVar (IdR p nome)) = getVar nome

chequeUnOp :: String -> OpUn -> Tipo -> CheqM Tipo
chequeUnOp s op t = comContexto s $ do
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

chequeOpBin :: String -> OpBin -> Tipo -> Tipo -> CheqM Tipo
chequeOpBin c op t1 t2 = comContexto c $ do
  let tipos_nums = all (`Set.member` numericos) [t1, t2]
  let tipos_iguais = t1 == t2
  let op_numerica = op `elem` [Soma, Mul, Div, Exp, Mod]
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

comContexto :: String -> CheqM a -> CheqM a
comContexto s c = 
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
    getVar' nome (v:vs) = case Map.lookup nome v of
      Just (_, tipo) -> pure tipo
      Nothing -> getVar' nome vs
    getVar' nome [] = throwError (NaoDeclarado nome)

