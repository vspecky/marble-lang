module Main where

import qualified Data.Map as Map
import Interpreter
import Interpreter.Environment
import Parser
import qualified Parser.Tracker as Tracker
import System.Environment (getArgs)
import Utils

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      program <- readFile filepath
      let trck = Tracker.new program
      let parsed = runStateT programP trck
      case parsed of
        Left err -> print err
        Right (program, _) -> do
          (err, env) <-
            runStateT (runEitherT $ evalProgram program) (Environment Map.empty)
          case err of
            Left e -> print e >> print env
            Right _ -> pure ()
    _ -> print "Usage: ./marble <path_to_file>"
