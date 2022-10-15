module Parser where

import Control.Applicative
import Data.Char
import Parser.AST
import Parser.Error
import qualified Parser.Tracker as Tracker
import Utils

type Parser a = StateT Tracker.Tracker (Either ParseError) a

-- Utility Parsers --
ws :: Parser ()
ws = do
  trck <- getState
  setState $ Tracker.advanceWhile isSpace trck

skipWs :: Parser a -> Parser a
skipWs p = ws *> p

-- Low Level/Token Parsers --
stringParser :: String -> Parser String
stringParser str =
  skipWs $ do
    trck <- getState
    if Tracker.startsWith str trck
      then do
        let trck' = Tracker.advanceN (length str) trck
        setState trck'
        pure str
      else StateT $ \_ -> Left (Expected str)

maybeStringParser :: String -> Parser (Maybe String)
maybeStringParser str =
  skipWs $ do
    trck <- getState
    if Tracker.startsWith str trck
      then do
        let trck' = Tracker.advanceN (length str) trck
        setState trck'
        pure $ Just str
      else pure Nothing

tokenParser :: String -> a -> ParseError -> Parser a
tokenParser str a err = do
  res <- maybeStringParser str
  case res of
    Just _ -> return a
    Nothing -> StateT $ \_ -> Left err

whilePredicateP :: (Char -> Bool) -> ParseError -> Parser String
whilePredicateP f err =
  skipWs $ do
    trck <- getState
    let parsed = Tracker.getWhile f trck
    if null parsed
      then StateT $ \_ -> Left err
      else do
        let trck' = Tracker.advanceN (length parsed) trck
        setState trck'
        pure parsed

eofParser :: Parser ()
eofParser =
  skipWs $ do
    trck <- getState
    if Tracker.trckCod trck == ""
      then pure ()
      else StateT $ \_ -> Left $ Expected "EOF"

operatorTP :: String -> Operator -> Parser Operator
operatorTP s op = tokenParser s op $ Expected s

opPlusTP :: Parser Operator
opPlusTP = operatorTP "+" OpPlus

opMinusTP :: Parser Operator
opMinusTP = operatorTP "-" OpMinus

opMultiplyTP :: Parser Operator
opMultiplyTP = operatorTP "*" OpMultiply

opDivideTP :: Parser Operator
opDivideTP = operatorTP "/" OpDivide

opEqualTP :: Parser Operator
opEqualTP = operatorTP "==" OpEqual

opGreaterThanTP :: Parser Operator
opGreaterThanTP = operatorTP ">" OpGreaterThan

opGreaterThanEqTP :: Parser Operator
opGreaterThanEqTP = operatorTP ">=" OpGreaterThanEq

opLesserThanTP :: Parser Operator
opLesserThanTP = operatorTP "<" OpLesserThan

opLesserThanEqTP :: Parser Operator
opLesserThanEqTP = operatorTP "<=" OpLesserThanEq

opDoubleInvCommasTP :: Parser Char
opDoubleInvCommasTP = tokenParser "\"" '"' $ Expected "Double inverted comma"

mIntTP :: Parser Atom
mIntTP = fmap (MInt . read) $ whilePredicateP isDigit $ Expected "Number"

mStrTP :: Parser Atom
mStrTP =
  opDoubleInvCommasTP *>
  fmap MStr (whilePredicateP (/= '"') $ Expected "String") <*
  opDoubleInvCommasTP

mBoolTP :: Parser Atom
mBoolTP =
  fmap (const $ MBool True) (stringParser "true") <|>
  fmap (const $ MBool False) (stringParser "false")

nullTP :: Parser Atom
nullTP = tokenParser "null" MNull $ Expected "null"

identNameTP :: Parser String
identNameTP = whilePredicateP isLetter $ Expected "Identifier"

identifierTP :: Parser Atom
identifierTP = Identifier <$> identNameTP

-- Parser Combinators --
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

-- Grammar Parsers --
anyOperatorP :: Parser Operator
anyOperatorP = opPlusTP <|> opMinusTP <|> opMultiplyTP <|> opDivideTP

funcApplicationP :: Parser Atom
funcApplicationP = do
  fname <- identNameTP
  stringParser "("
  args <- zeroOrMoreOf expressionP
  stringParser ")"
  pure $ FuncApplication fname args

atomP :: Parser Atom
atomP =
  mIntTP <|> mStrTP <|> mBoolTP <|> nullTP <|> funcApplicationP <|> identifierTP

factorBranchP :: Parser (Operator, Atom)
factorBranchP = do
  op <- opMultiplyTP <|> opDivideTP
  atom <- atomP
  pure (op, atom)

factorsP :: Parser Factors
factorsP = do
  atom <- atomP
  branches <- zeroOrMoreOf factorBranchP
  pure $ Factors atom branches

termBranchP :: Parser (Operator, Factors)
termBranchP = do
  op <- opPlusTP <|> opMinusTP
  factors <- factorsP
  pure (op, factors)

termsP :: Parser Terms
termsP = do
  factors <- factorsP
  branches <- zeroOrMoreOf termBranchP
  pure $ Terms factors branches

expressionBranchP :: Parser (Operator, Terms)
expressionBranchP = do
  op <-
    opEqualTP <|> opGreaterThanEqTP <|> opGreaterThanTP <|> opLesserThanEqTP <|>
    opLesserThanTP
  terms <- termsP
  pure (op, terms)

expressionP :: Parser Expression
expressionP = do
  terms <- termsP
  branch <- maybeOf expressionBranchP
  pure $ Expression terms branch

returnP :: Parser Statement
returnP = stringParser "return" *> (Return <$> expressionP) <* stringParser ";"

varDeclP :: Parser Statement
varDeclP = do
  stringParser "let"
  name <- identNameTP
  stringParser "="
  VarDecl name <$> expressionP <* stringParser ";"

varMutateP :: Parser Statement
varMutateP = do
  name <- identNameTP
  stringParser "="
  VarMutate name <$> expressionP <* stringParser ";"

statementBlockP :: Parser [Statement]
statementBlockP =
  stringParser "{" *> zeroOrMoreOf statementP <* stringParser "}"

ifElseP :: Parser Statement
ifElseP = do
  stringParser "if"
  condition <- expressionP
  ifBlock <- statementBlockP
  maybeElseBlock <- maybeOf (stringParser "else" *> statementBlockP)
  pure $ If condition ifBlock maybeElseBlock

statementP :: Parser Statement
statementP = returnP <|> varDeclP <|> varMutateP <|> ifElseP

funcDeclP :: Parser TopLevel
funcDeclP = do
  stringParser "func"
  funcName <- identNameTP
  stringParser "("
  firstArg <- maybeOf identNameTP
  restArgs <- zeroOrMoreOf (stringParser "," *> identNameTP)
  stringParser ")"
  funcBody <- statementBlockP
  pure $
    case firstArg of
      Just arg -> FuncDecl funcName (arg : restArgs) funcBody
      Nothing -> FuncDecl funcName [] funcBody

tlExprP :: Parser TopLevel
tlExprP = TLExpr <$> expressionP <* stringParser ";"

tlStatementP :: Parser TopLevel
tlStatementP = TLStatement <$> statementP

topLevelP :: Parser TopLevel
topLevelP = funcDeclP <|> tlStatementP <|> tlExprP

programP :: Parser Program
programP = Program <$> oneOrMoreOf topLevelP
