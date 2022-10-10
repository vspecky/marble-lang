module Parser where

import Control.Applicative
import Parser.AST
import Parser.Error
import Parser.State
import qualified Parser.Tracker as Tracker

type Parser a = StateT Tracker.Tracker (Either ParseError) a

isWhitespace :: Char -> Bool
isWhitespace c = c == '\n' || c == ' '

-- Low Level/Token Parsers --
stringParser :: String -> Parser (Maybe String)
stringParser str = do
  trck <- getState
  if Tracker.startsWith str trck
    then do
      let trck' = Tracker.advanceN (length str) trck
      setState trck'
      pure $ Just str
    else pure Nothing

tokenParser :: String -> a -> ParseError -> Parser a
tokenParser str a err = do
  res <- stringParser str
  case res of
    Just _ -> return a
    Nothing -> StateT $ \_ -> Left err

opPlusTP :: Parser Operator
opPlusTP = tokenParser "+" OpPlus $ Expected "+"

opMinusTP :: Parser Operator
opMinusTP = tokenParser "-" OpMinus $ Expected "-"

opMultiplyTP :: Parser Operator
opMultiplyTP = tokenParser "*" OpMultiply $ Expected "*"

opDivideTP :: Parser Operator
opDivideTP = tokenParser "/" OpDivide $ Expected "/"

-- Parser Combinators --
zeroOrMoreOf :: Parser a -> Parser [a]
zeroOrMoreOf = some

oneOrMoreOf :: Parser a -> Parser [a]
oneOrMoreOf = many

-- Grammar Parsers --
anyOperatorP :: Parser Operator
anyOperatorP = opPlusTP <|> opMinusTP <|> opMultiplyTP <|> opDivideTP
