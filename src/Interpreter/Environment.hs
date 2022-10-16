module Interpreter.Environment where

import qualified Data.Map as Map
import Parser.AST

data EnvEntry
  = MInterpFunc FuncName [IdentName] [Statement]
  | MInterpInt Int
  | MInterpStr String
  | MInterpNull
  | MInterpBool Bool
  deriving (Show)

newtype Environment =
  Environment
    { unEnv :: Map.Map String EnvEntry
    }
  deriving (Show)
