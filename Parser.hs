module Parser where

import Lexer
import Repr
import Text.Parsec
import Text.Parsec.Expr

token' :: TokenKind -> Parsec [Token] st Token
token' (LitInt _) = tokenPrim show nextPos test
   where
    test t@(Token _ (LitInt _)) = Just t
    test _ = Nothing
    nextPos pos _ _     = pos

token' (Id _) = tokenPrim show nextPos test
   where
    test t@(Token _ (Id _)) = Just t
    test _ = Nothing
    nextPos pos _ _     = pos

token' k = tokenPrim show nextPos test
   where
    test t@(Token _ k') = if k == k' then Just t else Nothing
    nextPos pos _ _     = pos

parens :: Parsec [Token] st a -> Parsec [Token] st a
parens p = do
  token' ParEsq
  t <- p
  token' ParDir
  return t

-- Documentação do buildExpressionParser:
-- https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Expr.html
expr :: Parsec [Token] st Expr
expr = buildExpressionParser table term
  where
    table =
      [ [ Infix (token' VezesVezes >> pure (EOpBin Exp)) AssocRight
        ]
      , [ Infix (token' Vezes  >> pure (EOpBin Mul)) AssocLeft
        , Infix (token' Divide >> pure (EOpBin Div)) AssocLeft
        , Infix (token' Porcento >> pure (EOpBin Mod)) AssocLeft
        ]
      , [ Infix (token' Mais  >> pure (EOpBin Soma)) AssocLeft
        , Infix (token' Menos >> pure (EOpBin Sub )) AssocLeft
        ]
      , [ Prefix (token' Menos  >> pure (EOpUn Neg))
        ]
      ]

    term  = 
      EInt <$> (token' (LitInt 0))
      <|> EString <$> (token' (LitString ""))
      <|> parens expr 
      <|> EChamada <$> (token' (Id "")) <*> (token' ParEsq *> (sepBy expr (token' Virgula)) <* token' ParDir)
      <|> EVar <$> (token' (Id ""))
  
