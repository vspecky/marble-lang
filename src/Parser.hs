module Parser
  ( programP
  ) where

import Control.Applicative
import Data.Char
import Parser.AST
import Parser.Error
import qualified Parser.Tracker as Tracker
import Utils

type Parser = StateT Tracker.Tracker (Either ParseError)

-----------------------------
-- General Utility Parsers --
-----------------------------
ws :: Parser ()
ws = do
  trck <- getState
  setState $ Tracker.advanceWhile isSpace trck

skipWs :: Parser a -> Parser a
skipWs p = ws *> p

getStateAndPos :: Parser (Tracker.Tracker, Pos)
getStateAndPos = do
  t <- getState
  pure (t, Tracker.toPos t)

getPos :: Parser Pos
getPos = Tracker.toPos <$> getState

withPos :: Parser (Pos -> a) -> Parser a
withPos p =
  skipWs $ do
    nowPos <- getPos
    ($ nowPos) <$> p

-----------------------------
-- Low Level/Token Parsers --
-----------------------------
string :: String -> Parser (String, Pos)
string str =
  skipWs $ do
    (trck, pos) <- getStateAndPos
    if Tracker.startsWith str trck
      then do
        let trck' = Tracker.advanceN (length str) trck
        setState trck'
        pure (str, pos)
      else StateT $ \_ -> Left (Expected str)

maybeString :: String -> Parser (Maybe (String, Pos))
maybeString str =
  skipWs $ do
    (trck, pos) <- getStateAndPos
    if Tracker.startsWith str trck
      then do
        let trck' = Tracker.advanceN (length str) trck
        setState trck'
        pure $ Just (str, pos)
      else pure Nothing

whilePredicate :: (Char -> Bool) -> ParseError -> Parser (String, Pos)
whilePredicate f err =
  skipWs $ do
    (trck, pos) <- getStateAndPos
    let parsed = Tracker.getWhile f trck
    if null parsed
      then StateT $ const (Left err)
      else do
        let trck' = Tracker.advanceN (length parsed) trck
        setState trck'
        pure (parsed, pos)

token :: String -> (Pos -> a) -> ParseError -> Parser a
token str fa err = do
  res <- maybeString str
  case res of
    Just (_, pos) -> pure $ fa pos
    Nothing -> StateT $ const (Left err)

eof :: Parser ()
eof =
  skipWs $ do
    trck <- getState
    if Tracker.trckCod trck == ""
      then pure ()
      else StateT $ \_ -> Left $ Expected "EOF"

---------------
-- Operators --
---------------
operator :: String -> (Pos -> Operator) -> Parser Operator
operator s op = token s op $ Expected s

opPlus :: Parser Operator
opPlus = operator "+" OpPlus

opMinus :: Parser Operator
opMinus = operator "-" OpMinus

opMultiply :: Parser Operator
opMultiply = operator "*" OpMultiply

opDivide :: Parser Operator
opDivide = operator "/" OpDivide

opEqual :: Parser Operator
opEqual = operator "==" OpEqual

opGreaterThan :: Parser Operator
opGreaterThan = operator ">" OpGreaterThan

opGreaterThanEq :: Parser Operator
opGreaterThanEq = operator ">=" OpGreaterThanEq

opLesserThan :: Parser Operator
opLesserThan = operator "<" OpLesserThan

opLesserThanEq :: Parser Operator
opLesserThanEq = operator "<=" OpLesserThanEq

opBoolAnd :: Parser Operator
opBoolAnd = operator "&&" OpBoolAnd

opBoolOr :: Parser Operator
opBoolOr = operator "||" OpBoolOr

-----------------
-- Punctuation --
-----------------
puncLparen :: Parser (String, Pos)
puncLparen = string "("

puncRparen :: Parser (String, Pos)
puncRparen = string ")"

puncLbrace :: Parser (String, Pos)
puncLbrace = string "{"

puncRbrace :: Parser (String, Pos)
puncRbrace = string "}"

puncComma :: Parser (String, Pos)
puncComma = string ","

puncDoubleInvComma :: Parser (String, Pos)
puncDoubleInvComma = string "\""

puncSemicolon :: Parser (String, Pos)
puncSemicolon = string ";"

puncAssignment :: Parser (String, Pos)
puncAssignment = string "="

--------------
-- Keywords --
--------------
keywordReturn :: Parser (String, Pos)
keywordReturn = string "return"

keywordLet :: Parser (String, Pos)
keywordLet = string "let"

keywordIf :: Parser (String, Pos)
keywordIf = string "if"

keywordElse :: Parser (String, Pos)
keywordElse = string "else"

keywordFunc :: Parser (String, Pos)
keywordFunc = string "func"

------------------------
-- Primitive Literals --
------------------------
mInt :: Parser Atom
mInt =
  fmap (\(str, pos) -> MInt (read str) pos) $
  whilePredicate isDigit $ Expected "Number"

mStr :: Parser Atom
mStr =
  string "\"" *> (uncurry MStr <$> whilePredicate (/= '"') (Expected "String")) <*
  string "\""

mBool :: Parser Atom
mBool =
  (MBool True . snd <$> string "true") <|>
  (MBool False . snd <$> string "false")

mNull :: Parser Atom
mNull = token "null" MNull $ Expected "null"

----------------------
-- Type Annotations --
----------------------
typeUtil :: String -> (Pos -> Type) -> Parser Type
typeUtil s ty = token s ty $ Expected s

typeInt :: Parser Type
typeInt = typeUtil "Int" IntType

typeBool :: Parser Type
typeBool = typeUtil "Bool" BoolType

typeString :: Parser Type
typeString = typeUtil "String" StringType

typeNull :: Parser Type
typeNull = typeUtil "Null" NullType

-----------------
-- Identifiers --
-----------------
identName :: Parser (String, Pos)
identName = whilePredicate isLetter (Expected "Identifier")

identifier :: Parser Atom
identifier = uncurry Identifier <$> identName

------------------------
-- Parser Combinators --
------------------------
zeroOrMoreOf :: Parser a -> Parser [a]
zeroOrMoreOf = many

oneOrMoreOf :: Parser a -> Parser [a]
oneOrMoreOf = some

maybeOf :: Parser a -> Parser (Maybe a)
maybeOf p =
  StateT $ \s ->
    pure $
    case runStateT p s of
      Right (res, s') -> (Just res, s')
      Left _ -> (Nothing, s)

surrounded :: Parser a -> Parser b -> Parser c -> Parser c
surrounded leftS rightS toParse = leftS *> toParse <* rightS

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy s = surrounded s s

---------------
-- Utilities --
---------------
inParentheses :: Parser a -> Parser a
inParentheses = surrounded puncLparen puncRparen

inBraces :: Parser a -> Parser a
inBraces = surrounded puncLbrace puncRbrace

---------------------
-- Grammar Parsers --
---------------------
mType :: Parser Type
mType = typeInt <|> typeBool <|> typeString <|> typeNull

identWithType :: Parser (IdentName, Type)
identWithType = do
  (iname, _) <- identName
  string "::"
  itype <- mType
  pure (iname, itype)

funcApplication :: Parser Atom
funcApplication =
  withPos $ do
    (fname, _) <- identName
    puncLparen
    firstArg <- maybeOf expression
    restArgs <- zeroOrMoreOf (puncComma *> expression)
    puncRparen
    case firstArg of
      Nothing -> pure $ FuncApplication fname []
      Just arg -> pure $ FuncApplication fname (arg : restArgs)

atom :: Parser Atom
atom =
  mInt <|> mStr <|> mBool <|> mNull <|> funcApplication <|> identifier <|>
  withPos (inParentheses (Nested <$> expression))

factorBranch :: Parser (Operator, Atom)
factorBranch = do
  op <- opMultiply <|> opDivide
  atom <- atom
  pure (op, atom)

factors :: Parser Factors
factors =
  withPos $ do
    atom <- atom
    branches <- zeroOrMoreOf factorBranch
    pure $ Factors atom branches

termBranch :: Parser (Operator, Factors)
termBranch = do
  op <- opPlus <|> opMinus
  factors <- factors
  pure (op, factors)

terms :: Parser Terms
terms =
  withPos $ do
    factors <- factors
    branches <- zeroOrMoreOf termBranch
    pure $ Terms factors branches

comparisonBranch :: Parser (Operator, Terms)
comparisonBranch = do
  op <-
    opEqual <|> opGreaterThanEq <|> opGreaterThan <|> opLesserThanEq <|>
    opLesserThan
  terms <- terms
  pure (op, terms)

comparison :: Parser Comparison
comparison =
  withPos $ do
    terms <- terms
    branch <- maybeOf comparisonBranch
    pure $ Comparison terms branch

expressionBranch :: Parser (Operator, Comparison)
expressionBranch = do
  op <- opBoolAnd <|> opBoolOr
  comparison <- comparison
  pure (op, comparison)

expression :: Parser Expression
expression =
  withPos $ do
    comparison <- comparison
    branches <- zeroOrMoreOf expressionBranch
    pure $ Expression comparison branches

returnStatement :: Parser Statement
returnStatement =
  withPos (surrounded keywordReturn puncSemicolon (Return <$> expression))

variableDeclaration :: Parser Statement
variableDeclaration =
  withPos $ do
    keywordLet
    (name, _) <- identName
    puncAssignment
    VarDecl name <$> expression <* puncSemicolon

variableMutation :: Parser Statement
variableMutation =
  withPos $ do
    (name, _) <- identName
    puncAssignment
    VarMutate name <$> expression <* puncSemicolon

statementBlock :: Parser [Statement]
statementBlock = inBraces (zeroOrMoreOf statement)

ifElse :: Parser Statement
ifElse =
  withPos $ do
    keywordIf
    condition <- expression
    ifBlock <- statementBlock
    maybeElseBlock <- maybeOf (keywordElse *> statementBlock)
    pure $ If condition ifBlock maybeElseBlock

statement :: Parser Statement
statement =
  returnStatement <|> variableDeclaration <|> variableMutation <|> ifElse

functionDeclaration :: Parser TopLevel
functionDeclaration =
  withPos $ do
    keywordFunc
    (funcName, _) <- identName
    puncLparen
    firstArg <- maybeOf identName
    restArgs <- fmap fst <$> zeroOrMoreOf (puncComma *> identName)
    puncRparen
    retType <- maybeOf (string "::" *> mType)
    funcBody <- statementBlock
    pure $
      case firstArg of
        Just (arg, _) -> FuncDecl funcName (arg : restArgs) funcBody
        Nothing -> FuncDecl funcName [] funcBody

topLevelExpression :: Parser TopLevel
topLevelExpression = withPos (TLExpr <$> (expression <* puncSemicolon))

topLevelStatement :: Parser TopLevel
topLevelStatement = withPos (TLStatement <$> statement)

topLevel :: Parser TopLevel
topLevel = functionDeclaration <|> topLevelStatement <|> topLevelExpression

programP :: Parser Program
programP = Program <$> oneOrMoreOf topLevel
