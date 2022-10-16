module Interpreter.Error where

import Utils

data InterpError
  = NoError
  | DivisionByZero
  | NotFoundInScope String
  | RuntimeError String
  | InterpreterErr String
  deriving (Show)

throw :: Monad m => InterpError -> EitherT InterpError m a
throw = EitherT . pure . Left
