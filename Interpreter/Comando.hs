module Interpreter.Comando where

import Control.Monad.State
import Control.Monad.Except
import Data.Functor
import Data.List (intercalate)

import GHC.Float (float2Double, double2Float)

import Interpreter.Basic
import Interpreter.Erro

import Repr
import Parser (chamadaP)

-- (>>=) quer dizer: a coisa da esquerda gera valor. Pegue esse valor e use como argumento na coisa da direta
-- (>>) quer dizer: a coisa da esquerda gera valor. Ignore esse valor e retorne seja lá o que a coisa da direita retornar.
--
-- ghci> retorne 3 >>= print
-- 3
--
-- ghci> ghci retorne 3 >> print "oi!"
-- oi

-- Os comandos em geral não produzem valor.
-- O RETORNE produz valor.
-- Os comandos que tem comandos dentro deles talvez produzem valor
-- (Se tiverem um retorne lá dentro).
evalCmds :: [Comando] -> EvalM (Maybe Valor)
evalCmds [] = pure Nothing
evalCmds (cmd:cmds) = case cmd of
  (Atribuicao p lv e) -> 
    evalAtribuicao p lv e >> evalCmds cmds

  (ImprimaCmd _ e) -> 
    (evalExpr e >>= liftIO . print) >> evalCmds cmds

  (Inicializacao _ i t e) ->
    (evalExpr e >>= addVar i) >> evalCmds cmds

  (ChamadaCmd p nome args) -> 
    getProc nome >>= (`evalProcedimento` args) >> evalCmds cmds

  (EnquantoCmd p uncs) -> do
    scp <- novoBloco "ENQUANTO"

    let go [] = pure Nothing

        go ((e,cmds'):uncs') = do
          v <- evalExpr e
          case v of
            VBool True -> do
              comEscopo id scp (evalCmds cmds')
              go uncs

            VBool False -> go uncs'

            _ -> throwError (TypeError p)

    retorno <- go uncs
    case retorno of
      Nothing -> evalCmds cmds
      v -> return v

  (SeCmd p uncs) -> do
    scp <- novoBloco "SE"

    let go [] = throwError (UnexaustivePatterns p)

        go ((e,cmds'):uncs') = do
          v <- evalExpr e
          case v of
            VBool True -> do
              comEscopo id scp (evalCmds cmds')

            VBool False -> go uncs'

            _ -> throwError (TypeError p)

    retorno <- go uncs
    case retorno of
      Nothing -> evalCmds cmds
      v -> return v

  (CasamentoCmd p noivo bracos) -> comPosicao p $ do
    scp <- novoBloco "CASAMENTO"
    comEscopo id scp $ do 
      v <- evalExpr noivo
      retorno <- evalBraco v bracos
      case retorno of
        Nothing -> evalCmds cmds
        v -> return v

  (RetorneCmd _ e) -> Just <$> evalExpr e

evalBraco :: Valor -> [(Padrao, [Comando])] -> EvalM (Maybe Valor)
evalBraco _ [] = error "Padrão não existe"
evalBraco v ((Padrao variante captura, cmds) : resto) = 
  case v of
    VEnum nome valores ->
      if nome == variante
      then do
        case (captura, valores) of
          (Just var, [val]) -> addVar var val
          _ -> return ()
        evalCmds cmds
      else evalBraco v resto
    _ -> error (show v ++ " não é um enum")

evalAtribuicao :: Pos -> Atribuendo -> Expr -> EvalM ()
evalAtribuicao p lv e = do
  rv <- evalExpr e
  case lv of
    AId nome -> modificarVar nome rv $> ()
    AArray arrayNome idxExpr -> do
      -- Buscar a lista/tupla/dicionário
      arrayVal <- getValue arrayNome
      idxVal <- evalExpr idxExpr
      
      -- Criar nova estrutura com o valor modificado
      newArrayVal <- case (arrayVal, idxVal) of
        (VList xs, VInt i)
          | i >= 0 && i < length xs -> 
              return $ VList (take i xs ++ [rv] ++ drop (i+1) xs)
          | otherwise -> throwError IndexOutOfBounds
        
        (VTuple xs, VInt i)
          | i >= 0 && i < length xs ->
              return $ VTuple (take i xs ++ [rv] ++ drop (i+1) xs)
          | otherwise -> throwError IndexOutOfBounds
        
        (VDict pairs, VString key) ->
          let newPairs = map (\(k,v) -> if k == VString key then (k, rv) else (k,v)) pairs
          in return $ VDict newPairs
        
        _ -> throwError $ TypeError p
      
      -- Atualizar a variável original
      modificarVar arrayNome newArrayVal $> ()
    
    (AEstrutura nome campo) -> do
      velha <- getValue nome
      case velha of
        VEstrutura campos ->
          if any ((== campo) . fst) campos
            then do
              let novosCampos =
                    map (\(c,v) -> if c == campo then (c,rv) else (c,v)) campos
              modificarVar nome (VEstrutura novosCampos)
              pure ()
            else
              error ("Campo desconhecido: " ++ show campo)

    ARef _ -> throwError FaltaImplementar

-- Joga fora o retorno de um procedimento
evalProcedimento :: ProcedimentoR -> [Expr] -> EvalM ()
evalProcedimento (ProcedimentoR p ident pars cmds) args =
  void (evalSubprograma (p, ident, pars, cmds, args))

-- Se uma função não encontrou retorno,
-- panic!!
evalFuncao :: FuncaoR -> [Expr] -> EvalM Valor
evalFuncao (FuncaoR p i pars _ cmds) args = do
  retorno <- evalSubprograma (p, i, pars, cmds, args)
  case retorno of
    Just v -> return v
    Nothing -> error "Função sem retorno"

-- Isso aqui tá feio e mal feito.
-- Se tiver algum erro aqui nessa parte, 
-- a recomendação da OMS é rezar.
-- Se for Umberto, por favor pule para a função seguinte.
evalSubprograma :: (Pos, Id, [Parametro], [Comando], [Expr]) -> EvalM (Maybe Valor)
evalSubprograma (p, IdR _ nome , pars, cs, es) = do
  vs <- mapM evalExpr es
  comEscopo (const ["main"]) nome $ do
    add pars (zip vs es) -- Inicializa os parâmetros na memória
    evalCmds cs -- Avalia os comandos
  where
    add :: [Parametro] -> [(Valor, Expr)] -> EvalM ()
    add ((Parametro n _ porref) : ps) ((v, e) : ves) 
      | porref = case e of
        EVar indent -> do
          v' <- getRaw indent
          case v' of
            (VRef endereco) -> do
              addVar n (VRef endereco)
              add ps ves
            _ -> do
              scp <- resolveVar indent
              addVar n (VRef (scp, indent))
        _ -> throwError FaltaImplementar
      | otherwise = addVar n v *> add ps ves

    add [] [] = pure ()
    add _ _ = throwError IncorrectNumberOfParameters

evalLit :: Lit -> EvalM Valor
evalLit (LInt _ x) = pure $ VInt x
evalLit (LBool _ b) = pure $ VBool b
evalLit (LString _ s) = pure $ VString s
evalLit (LNada _) = pure VNada
evalLit (LReal _ r) = pure $ VReal r
evalLit (LFloat _ f) = pure $ VFloat f

-- Toda expressão produz valor
evalExpr :: Expr -> EvalM Valor
evalExpr (ELit l) = evalLit l
evalExpr (EVar id) = getValue id
evalExpr (ELeia p) = VString <$> liftIO getLine

evalExpr (EChamada p ident args) = 
  getFunc ident >>= (`evalFuncao` args)

evalExpr (EIndice p container idx) = do
  containerVal <- evalExpr container
  idxVal <- evalExpr idx
  case (containerVal, idxVal) of
    (VList xs, VInt i) 
      | i >= 0 && i < length xs -> return (xs !! i)
      | otherwise -> throwError IndexOutOfBounds
    
    (VTuple xs, VInt i)
      | i >= 0 && i < length xs -> return (xs !! i)
      | otherwise -> throwError IndexOutOfBounds
          
    (VDict pairs, VString key) ->
      case lookup (VString key) pairs of
        Just v -> return v
        Nothing -> throwError KeyNotFound
    
    _ -> throwError $ TypeError p

evalExpr (EList _ elems) = do
  vs <- mapM evalExpr elems
  return $ VList vs
evalExpr (ETuple _ elems) = do
  vs <- mapM evalExpr elems
  return $ VTuple vs
evalExpr (EDict _ pairs) = do
  ps <- mapM (\(k,v) -> do
                kk <- evalExpr k
                vv <- evalExpr v
                return (kk, vv)) pairs
  return $ VDict ps

evalExpr (EEnum pos enum variante me) = case me of
  (Just e) -> do
    v <- evalExpr e
    return (VEnum variante [v])
  Nothing -> return (VEnum variante [])

evalExpr (EEstrutura _ campos) = do
  let (ids, es) = unzip campos
  vs <- mapM evalExpr es
  return (VEstrutura (zip ids vs))

evalExpr (EAcesso _ nome campo) = do
  est <- getValue nome
  case est of
    (VEstrutura campos) -> do
      case lookup campo campos of
        Just valor -> return valor
        Nothing -> error (show nome ++ " não contém campo " ++ show campo)
    _ -> error (show nome ++ " não é uma estrutura")

evalExpr (EOpBin p op e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case evalOpBin op v1 v2 of
    Just v -> pure v
    Nothing -> throwError $ TypeError p

evalExpr (EOpUn p op e1) = do
  v1 <- evalExpr e1
  case evalOpUn op v1 of
    Just v -> pure v
    Nothing -> throwError $ TypeError p


evalOpBin :: OpBin -> Valor -> Valor -> Maybe Valor
evalOpBin op (VInt x) (VInt y) = Just $ case op of
  Soma -> VInt (x + y)
  Sub -> VInt (x - y)
  Mul -> VInt (x * y)
  Div -> VInt (x `div` y)
  Exp -> VInt (x ^ y)
  Mod -> VInt (x `mod` y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)
  DiferenteOp -> VBool ( x /= y )

evalOpBin op (VReal x) (VReal y) = Just $ case op of
  Soma -> VReal (x + y)
  Sub -> VReal (x - y)
  Mul -> VReal (x * y)
  Div -> VReal (x / y)
  Exp -> VReal (x ** y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)
  DiferenteOp -> VBool (x /= y)

evalOpBin op (VFloat x) (VFloat y) = Just $ case op of
  Soma -> VFloat (x + y)
  Sub -> VFloat (x - y)
  Mul -> VFloat (x * y)
  Div -> VFloat (x / y)
  Exp -> VFloat (x ** y)
  Menor -> VBool (x < y)
  Maior -> VBool (x > y)
  MenorIgualOp -> VBool (x <= y)
  MaiorIgualOp -> VBool (x >= y)
  IgualOp -> VBool (x == y)
  DiferenteOp -> VBool (x /= y)

evalOpBin op (VBool b1) (VBool b2) = Just $ case op of
  AndOp -> VBool (b1 && b2)
  OrOp -> VBool (b1 || b2)

evalOpBin Soma (VString s1) (VString s2) = Just $ VString (s1 ++ s2) -- concatenaçao (talvez nao seja a a melhor maneira)

evalOpBin _ _ _ = Nothing

evalOpUn :: OpUn -> Valor -> Maybe Valor
evalOpUn Neg (VInt x) = Just $ VInt (-x)
evalOpUn NaoOp (VBool b) = Just $ VBool (not b)
evalOpUn (Conv TInt) v = Just $ case v of
  (VInt x) -> VInt x
  (VReal x) -> VInt (truncate x)
  (VFloat x) -> VInt (truncate x)
  (VBool True) -> VInt 1
  (VBool False) -> VInt 0
  (VString s) -> VInt (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn (Conv TBool) v = Just $ case v of
  (VInt 0) -> VBool False
  (VInt _) -> VBool True
  (VReal 0) -> VBool False
  (VReal _) -> VBool True
  (VFloat 0) -> VBool False
  (VFloat _) -> VBool True
  (VString "") -> VBool False
  (VString _) -> VBool True
  VNada -> VBool False

evalOpUn (Conv TReal) v = Just $ case v of
  (VInt x) -> VReal (fromIntegral x)
  (VFloat x) -> VReal (float2Double x)
  (VBool True) -> VReal 1
  (VBool False) -> VReal 0
  (VString s) -> VReal (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn (Conv TFloat) v = Just $ case v of
  (VInt x) -> VFloat (fromIntegral x)
  (VReal x) -> VFloat (double2Float x)
  (VBool True) -> VFloat 1
  (VBool False) -> VFloat 0
  (VString s) -> VFloat (read s) --- TODO: MUUUIITO ERRADO!!!

evalOpUn (Conv TString) v = Just $ case v of
  (VInt x) -> VString $ show x
  (VReal x) -> VString $ show x
  (VBool b) -> VString $ show b
  (VTuple xs) -> VString $ "(" ++ intercalate "," (map show xs) ++ ")" -- talvez seja meio gambiarra (estou fazendo para o p6 exclusivamente)
  VNada -> VString "nada"
