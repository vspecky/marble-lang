module Main where

import Parser
import qualified Parser.Tracker as Tracker
import Utils

main :: IO ()
main = do
  program <- readFile "./examples/proto_09-10-2022/fib.mrbl"
  let trck = Tracker.new program
  let parsed = runStateT programP trck
  print parsed
