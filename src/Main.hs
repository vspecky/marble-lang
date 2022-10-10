module Main where

import Parser
import Parser.State
import qualified Parser.Tracker as Tracker

main :: IO ()
main = do
  let trck = Tracker.new "/+*-"
  let parsed = runStateT (oneOrMoreOf anyOperatorP) trck
  print parsed
