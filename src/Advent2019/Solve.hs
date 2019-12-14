module Advent2019.Solve
  ( solve
  ) where

import qualified Data.ByteString.Lazy as LBS
import System.IO (hPutStrLn, stderr)
import Data.Either (Either(..))
import Data.Binary.Put (runPut, putCharUtf8, putStringUtf8)

import qualified Advent2019.Day1
import qualified Advent2019.Day2
import qualified Advent2019.Day3
import qualified Advent2019.Day4
import qualified Advent2019.Day5
import qualified Advent2019.Day7

solver :: Int -> IO (Either String (String, String))
solver 1 = Advent2019.Day1.solve
solver 2 = Advent2019.Day2.solve
solver 3 = Advent2019.Day3.solve
solver 4 = Advent2019.Day4.solve
solver 5 = Advent2019.Day5.solve
solver 7 = Advent2019.Day7.solve
solver n = pure . Left $ "Unknown problem " ++ show n

solve :: Int -> IO (LBS.ByteString)
solve k = do
  res <- solver k
  let bs = case res of
             Left err -> runPut $ (putStringUtf8 err) >> (putCharUtf8 '\n')
             Right (part1, part2) -> runPut $ do
                                       putStringUtf8 part1
                                       putCharUtf8 '\n'
                                       putStringUtf8 part2
                                       putCharUtf8 '\n'
  pure bs
