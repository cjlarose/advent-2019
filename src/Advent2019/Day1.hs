module Advent2019.Day1 (
  solve
  ) where

import Advent2019.Input (getInputAsString)

solve :: IO ()
solve = do
  inputAsString <- getInputAsString 1
  putStrLn inputAsString
