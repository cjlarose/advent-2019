module Advent2019.Input
  ( getProblemInputAsString
  , getProblemInputAsByteString
  ) where

import qualified Data.ByteString as B

path :: Int -> String
path problemNumber = "inputs/" ++ (show problemNumber) ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path

getProblemInputAsByteString :: Int -> IO B.ByteString
getProblemInputAsByteString = B.readFile . path
