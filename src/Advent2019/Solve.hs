module Advent2019.Solve
  ( solve
  ) where

import System.IO (hPutStrLn, stderr)

import qualified Advent2019.Day1
import qualified Advent2019.Day2
import qualified Advent2019.Day3
import qualified Advent2019.Day4

solve :: Int -> IO ()
solve 1 = Advent2019.Day1.solve
solve 2 = Advent2019.Day2.solve
solve 3 = Advent2019.Day3.solve
solve 4 = Advent2019.Day4.solve
solve n = hPutStrLn stderr $ "Unknown problem " ++ show n
