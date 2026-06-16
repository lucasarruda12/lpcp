module Interpreter where

import Repr

eval :: Expr -> Lit
eval (ELit l) = l

comandoI :: Comando -> IO ()
comandoI (ImprimaCmd _ e) = do
  let (LInt _ x) = eval e
  print x
