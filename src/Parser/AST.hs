module Parser.AST where

type FuncName = String

type IdentName = String

class MarbleType t where
  typeOf :: t -> String

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
  deriving (Show)

instance MarbleType Operator where
  typeOf _ = "Operator"

data Atom
  = MInt Int
  | MStr String
  | MBool Bool
  | MNull
  | FuncApplication FuncName [Expression]
  | Identifier IdentName
  | Nested Terms
  deriving (Show)

instance MarbleType Atom where
  typeOf a =
    case a of
      MInt _ -> "Int"
      MStr _ -> "String"
      MBool _ -> "Boolean"
      MNull -> "Null"
      _ -> "Atom"

data Factors =
  Factors Atom [(Operator, Atom)]
  deriving (Show)

data Terms =
  Terms Factors [(Operator, Factors)]
  deriving (Show)

data Expression =
  Expression Terms (Maybe (Operator, Terms))
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
