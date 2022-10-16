module Parser.AST where

type FuncName = String

type IdentName = String

data Operator
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpEqual
  | OpGreaterThan
  | OpGreaterThanEq
  | OpLesserThan
  | OpLesserThanEq
  | OpBoolAnd
  | OpBoolOr
  deriving (Show)

data Atom
  = MInt Int
  | MStr String
  | MBool Bool
  | MNull
  | FuncApplication FuncName [Expression]
  | Identifier IdentName
  | Nested Expression
  deriving (Show)

data Factors =
  Factors Atom [(Operator, Atom)]
  deriving (Show)

data Terms =
  Terms Factors [(Operator, Factors)]
  deriving (Show)

data Comparison =
  Comparison Terms (Maybe (Operator, Terms))
  deriving (Show)

data Expression =
  Expression Comparison [(Operator, Comparison)]
  deriving (Show)

data Statement
  = Return Expression
  | VarDecl IdentName Expression
  | VarMutate IdentName Expression
  | If Expression [Statement] (Maybe [Statement])
  deriving (Show)

data TopLevel
  = FuncDecl FuncName [IdentName] [Statement]
  | TLExpr Expression
  | TLStatement Statement
  deriving (Show)

newtype Program =
  Program [TopLevel]
  deriving (Show)
