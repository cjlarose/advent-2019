module Advent2019.Input (
  getInputAsString
  ) where

path :: Int -> String
path problemNumber = "inputs/" ++ (show problemNumber) ++ ".txt"

getInputAsString :: Int -> IO String
getInputAsString = readFile . path
