module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Advent2019.Solve (solve)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> solve . read $ arg
    _ -> hPutStrLn stderr $ "Usage: advent2019-exe problem-number"
