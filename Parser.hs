module Parser where

import Lexer
import Text.Parsec

token' :: Token -> Parsec [Token] st Token
token' t = tokenPrim show nextPos test
   where
     test t'        = if t == t' then Just t else Nothing
     nextPos pos _ _ = pos

