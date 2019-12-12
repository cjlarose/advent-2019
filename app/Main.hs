module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Advent2019.Day1
import qualified Advent2019.Day2
import qualified Advent2019.Day3
import qualified Advent2019.Day4

solver :: Int -> IO ()
solver 1 = Advent2019.Day1.solve
solver 2 = Advent2019.Day2.solve
solver 3 = Advent2019.Day3.solve
solver 4 = Advent2019.Day4.solve
solver n = hPutStrLn stderr $ "Unknown problem " ++ show n

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> solver . read $ arg
    _ -> hPutStrLn stderr $ "Usage: advent2019-exe problem-number"
