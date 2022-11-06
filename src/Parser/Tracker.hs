module Parser.Tracker where

import Utils

data Tracker =
  Tracker
    { trckCod :: String
    , trckIdx :: Int
    , trckLin :: Int
    , trckCol :: Int
    }
  deriving (Show)

new :: String -> Tracker
new code = Tracker code 0 1 1

advance :: Tracker -> Tracker
advance trck@Tracker {trckCod = []} = trck
advance (Tracker ('\n':cs) idx lin col) = Tracker cs (idx + 1) (lin + 1) 1
advance (Tracker (_:cs) idx lin col) = Tracker cs (idx + 1) lin (col + 1)

advanceN :: Int -> Tracker -> Tracker
advanceN 0 = id
advanceN 1 = advance
advanceN n = advance . advanceN (n - 1)

advanceWhile :: (Char -> Bool) -> Tracker -> Tracker
advanceWhile f trck@Tracker {trckCod = []} = trck
advanceWhile f trck@Tracker {trckCod = (x:_)} =
  if f x
    then advanceWhile f (advance trck)
    else trck

getWhile :: (Char -> Bool) -> Tracker -> String
getWhile f trck@Tracker {trckCod = []} = []
getWhile f trck@Tracker {trckCod = (x:xs)} =
  if f x
    then x : getWhile f (advance trck)
    else []

startsWith :: String -> Tracker -> Bool
startsWith str trck = str == take (length str) (trckCod trck)

toPos :: Tracker -> Pos
toPos (Tracker _ _ lin col) = Pos lin col
