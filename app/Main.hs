module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Advent2019.Solve (solve)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> (solve . read $ arg) >>= C.putStr
    _ -> hPutStrLn stderr $ "Usage: advent2019-exe problem-number"
