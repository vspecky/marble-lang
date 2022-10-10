module Parser.Error where

data ParseError
  = NoError
  | Expected String
  | ExpectedFound String String
  deriving (Show)

instance Semigroup ParseError where
  err <> NoError = err
  _ <> err = err

instance Monoid ParseError where
  mempty = NoError
