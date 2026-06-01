module Parser where

import Lexer
import Program
import Text.Parsec (Parsec(..), tokenPrim, chainl1, choice, many)

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

expr :: Parsec [Token] st Expr
expr = do
  f <- factor
  fs <- many $ do
    token' VezesVezes
    factor
  pure (foldl (BinOp Exp) f fs)
  where
    factor :: Parsec [Token] st Expr
    factor = Lit <$> (token' $ LitInt 0)
  
