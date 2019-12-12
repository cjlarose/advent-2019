module Advent2019.Day2
  ( solve
  , runProgram
  ) where

import Data.List (find)
import Data.Maybe (fromJust)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runProgram)

runProgramWithInputs :: [Int] -> (Int, Int) -> Int
runProgramWithInputs xs (a, b) = runProgram xs [(1, a), (2, b)]

findNounVerb :: [Int] -> Int -> (Int, Int)
findNounVerb xs n = fst . fromJust . find ((== n) . snd) . map (\p -> (p, runProgramWithInputs xs p)) $ [(a, b) | a <- [0..99], b <- [0..99]]

printResults :: [Int] -> IO ()
printResults xs = do
  let part1 = runProgramWithInputs xs (12, 2)
  let (noun, verb) = findNounVerb xs 19690720
  putStrLn . show $ part1
  putStrLn . show $ 100 * noun + verb

solve :: IO ()
solve = getProblemInputAsByteString 2 >>= withSuccessfulParse program printResults
