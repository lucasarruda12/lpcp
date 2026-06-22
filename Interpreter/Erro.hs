module Interpreter.Erro where

import Repr

data Erro 
  = TypeError Pos
  | UndefinedVariable Pos
  | AlreadyDefinedVariable Pos
  | IncorrectNumberOfParameters
  | UnexaustivePatterns Pos
  | Context Pos Erro
  | FaltaImplementar
  deriving (Show)

