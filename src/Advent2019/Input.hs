module Advent2019.Input (
  getProblemInputAsString
  ) where

path :: Int -> String
path problemNumber = "inputs/" ++ (show problemNumber) ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path
