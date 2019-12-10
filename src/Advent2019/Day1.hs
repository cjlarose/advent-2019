module Advent2019.Day1 (
  solve
  ) where

import Advent2019.Input (getProblemInputAsString)

solve :: IO ()
solve = do
  inputAsString <- getProblemInputAsString 1
  putStrLn inputAsString
