module Parser.AST where

import Utils

type FuncName = String

type IdentName = String

newtype Program =
  Program [TopLevel]
  deriving (Show)

data TopLevel
  = FuncDecl
      { fnDeclName :: FuncName
      , fnDeclArgs :: [IdentName]
      , fnDeclBody :: [Statement]
      , fnDeclPos :: Pos
      }
  | TLExpr Expression Pos
  | TLStatement Statement Pos
  deriving (Show)

data Statement
  = Return Expression Pos
  | VarDecl IdentName Expression Pos
  | VarMutate IdentName Expression Pos
  | If Expression [Statement] (Maybe [Statement]) Pos
  deriving (Show)

data Expression =
  Expression Comparison [(Operator, Comparison)] Pos
  deriving (Show)

data Comparison =
  Comparison Terms (Maybe (Operator, Terms)) Pos
  deriving (Show)

data Terms =
  Terms Factors [(Operator, Factors)] Pos
  deriving (Show)

data Factors =
  Factors Atom [(Operator, Atom)] Pos
  deriving (Show)

data Operator
  = OpPlus Pos
  | OpMinus Pos
  | OpMultiply Pos
  | OpDivide Pos
  | OpEqual Pos
  | OpGreaterThan Pos
  | OpGreaterThanEq Pos
  | OpLesserThan Pos
  | OpLesserThanEq Pos
  | OpBoolAnd Pos
  | OpBoolOr Pos
  deriving (Show)

data Type
  = IntType Pos
  | BoolType Pos
  | StringType Pos
  | NullType Pos
  deriving (Show)

data Atom
  = MInt Int Pos
  | MStr String Pos
  | MBool Bool Pos
  | MNull Pos
  | FuncApplication FuncName [Expression] Pos
  | Identifier IdentName Pos
  | Nested Expression Pos
  deriving (Show)
