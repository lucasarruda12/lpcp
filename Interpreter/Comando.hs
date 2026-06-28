module Interpreter.Comando where

import Control.Monad.State
import Control.Monad.Except
import Data.Functor
import Data.List (intercalate)

import GHC.Float (float2Double, double2Float)

import Interpreter.Basic
import Interpreter.Erro
import Interpreter.Expr

import Repr
import Parser (chamadaP)

instance Evaluavel Comando where
  eval (Atribuicao p lv e) = do
    rv <- eval e
    case lv of
      AId nome -> modificarVar nome rv $> VNada
      AArray arrayNome idxExpr -> do
        -- Buscar a lista/tupla/dicionário
        arrayVal <- getValue arrayNome
        idxVal <- eval idxExpr
        
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
        modificarVar arrayNome newArrayVal $> VNada
      
      (AEstrutura nome campo) -> do
        velha <- getValue nome
        case velha of
          VEstrutura campos ->
            if any ((== campo) . fst) campos
              then do
                let novosCampos =
                      map (\(c,v) -> if c == campo then (c,rv) else (c,v)) campos
                modificarVar nome (VEstrutura novosCampos)
                pure VNada
              else
                error ("Campo desconhecido: " ++ show campo)

      ARef _ -> throwError FaltaImplementar

  eval (ImprimaCmd _ e) = do
    v <- eval e
    liftIO $ print v
    return v

  eval (Inicializacao p i t e) = comPosicao p $ do
    v <- eval e
    addVar i v
    return VNada

  eval (ChamadaCmd p nome args) = comPosicao p $ do
    (ProcedimentoR p i pars cs) <- getProc nome
    eval (p, i, pars, cs, args)

  eval (EnquantoCmd p secs) = do
    scp <- novoBloco "ENQUANTO"

    let go [] = pure VNada

        go ((e,cmds):uncs) = do
          v <- eval e
          case v of
            VBool True -> do
              comEscopo id scp (mapM_ eval cmds)
              go secs

            VBool False -> go uncs

            _ -> throwError (TypeError p)

    go secs 
          

  eval (SeCmd p secs) = do
    scp <- novoBloco "SE"

    let go [] = throwError (UnexaustivePatterns p)

        go ((e,cmds):uncs) = do
          v <- eval e
          case v of
            VBool True -> do
              comEscopo id scp (mapM_ eval cmds)
              return VNada

            VBool False -> go uncs

            _ -> throwError (TypeError p)

    go secs 

  eval (RetorneCmd _ _) = error "Retorne fora de bloco"

  eval (CasamentoCmd p noivo bracos) = comPosicao p $ do
    scp <- novoBloco "CASAMENTO"
    comEscopo id scp (do 
      v <- eval noivo
      executarBraco v bracos)
    where
      executarBraco _ [] = throwError $ UnexaustivePatterns p
      executarBraco v ((Padrao variante captura, cmds) : resto) = 
        case v of
          VEnum nome valores ->
            if nome == variante
            then do
              case (captura, valores) of
                (Just var, [val]) -> addVar var val
                _ -> return ()
              eval cmds
            else executarBraco v resto
          _ -> error (show v ++ " não é um enum")
            
instance Evaluavel [Comando] where
  eval (cmd:cmds) = case cmd of
    (SeCmd p uncs) -> do
      scp <- novoBloco "SE"

      let go [] = throwError (UnexaustivePatterns p)

          go ((e,cmds'):uncs') = do
            v <- eval e
            case v of
              VBool True -> do
                comEscopo id scp (eval cmds')

              VBool False -> go uncs'

              _ -> throwError (TypeError p)

      go uncs 

    (EnquantoCmd p uncs) -> do
      scp <- novoBloco "ENQUANTO"

      let go [] = pure VNada

          go ((e,cmds'):uncs') = do
            v <- eval e
            case v of
              VBool True -> do
                comEscopo id scp (eval cmds')
                go uncs

              VBool False -> go uncs'

              _ -> throwError (TypeError p)

      go uncs 

    (RetorneCmd _ e) -> eval e


    (CasamentoCmd p noivo bracos) -> comPosicao p (do
      scp <- novoBloco "CASAMENTO"
      comEscopo id scp (do 
        v <- eval noivo
        executarBraco v bracos))

    _ -> eval cmd *> eval cmds

    where
      executarBraco _ [] = error "Padrão não existe"
      executarBraco v ((Padrao variante captura, cmds) : resto) = 
        case v of
          VEnum nome valores ->
            if nome == variante
            then do
              case (captura, valores) of
                (Just var, [val]) -> addVar var val
                _ -> return ()
              eval cmds
            else executarBraco v resto
          _ -> error (show v ++ " não é um enum")

  eval [] = pure VNada

instance Evaluavel (Pos, Id, [Parametro], [Comando], [Expr])
  where
  eval (p, IdR _ nome , pars, cs, es) = do
    vs <- mapM eval es
    comEscopo (const ["main"]) nome (do
      add pars (zip vs es) -- Inicializa os parâmetros na memória
      eval cs) -- Avalia os comandos
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

instance Evaluavel Lit where
  eval (LInt _ x) = pure $ VInt x
  eval (LBool _ b) = pure $ VBool b
  eval (LString _ s) = pure $ VString s
  eval (LNada _) = pure VNada
  eval (LReal _ r) = pure $ VReal r
  eval (LFloat _ f) = pure $ VFloat f

instance Evaluavel Expr where
  eval (ELit l) = eval l
  eval (EVar id) = getValue id

  eval (ELeia p) = VString <$> liftIO getLine

  eval (EChamada p i es) = do
    (FuncaoR p i pars _ cmds) <- getFunc i
    eval (p, i, pars, cmds, es)

  eval (EIndice p container idx) = do
    containerVal <- eval container
    idxVal <- eval idx
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

  eval (EList _ elems) = do
    vs <- mapM eval elems
    return $ VList vs
  eval (ETuple _ elems) = do
    vs <- mapM eval elems
    return $ VTuple vs
  eval (EDict _ pairs) = do
    ps <- mapM (\(k,v) -> do
                  kk <- eval k
                  vv <- eval v
                  return (kk, vv)) pairs
    return $ VDict ps

  eval (EEnum pos enum variante me) = case me of
    (Just e) -> do
      v <- eval e
      return (VEnum variante [v])
    Nothing -> return (VEnum variante [])

  eval (EEstrutura _ campos) = do
    let (ids, es) = unzip campos
    vs <- mapM eval es
    return (VEstrutura (zip ids vs))

  eval (EAcesso _ nome campo) = do
    est <- getValue nome
    case est of
      (VEstrutura campos) -> do
        case lookup campo campos of
          Just valor -> return valor
          Nothing -> error (show nome ++ " não contém campo " ++ show campo)
      _ -> error (show nome ++ " não é uma estrutura")

  eval (EOpBin p op e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case evalOpBin op v1 v2 of
      Just v -> pure v
      Nothing -> throwError $ TypeError p

  eval (EOpUn p op e1) = do
    v1 <- eval e1
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
