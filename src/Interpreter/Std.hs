module Interpreter.Std where

import Control.Monad
import qualified Data.Map as Map
import Interpreter.Environment
import Interpreter.Error
import Parser.AST
import Utils

type Interpreter = EitherT InterpError (StateT Environment IO)

type StdFunction = [Atom] -> Interpreter Atom

stdFnDict :: Map.Map String StdFunction
stdFnDict = Map.fromList [("print", stdPrint), ("string", stdString)]

showAtom :: Atom -> String
showAtom a =
  case a of
    MInt a _ -> show a
    MStr s _ -> s
    MBool b _ ->
      if b
        then "true"
        else "false"
    MNull _ -> "null"
    _ -> "<Unknown>"

verifyArgArity :: String -> Int -> Int -> Interpreter ()
verifyArgArity fnName expected given =
  if expected /= given
    then throw $
         RuntimeError $
         "Function '" <> fnName <> "' takes " <> show expected <>
         " arguments but " <>
         show given <>
         " were given"
    else pure ()

stdPrint :: [Atom] -> Interpreter Atom
stdPrint atoms = do
  forM_ atoms (lift2 . putStr . showAtom)
  lift2 $ putStr "\n"
  pure $ MNull noPos

stdString :: [Atom] -> Interpreter Atom
stdString atoms = do
  verifyArgArity "stdString" 1 (length atoms)
  case atoms of
    [a] -> pure $ MStr (showAtom a) noPos
    _ -> pure $ MNull noPos
