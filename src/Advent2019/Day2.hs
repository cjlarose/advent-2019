module Advent2019.Day2
  ( solve
  ) where

import Data.List (find)
import Data.Maybe (fromJust)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Machine (writeToAddress, valueAtAddress)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (withMachine, runMachine)

runProgramWithInputs :: [Int] -> (Int, Int) -> Int
runProgramWithInputs xs (a, b) = fst . withMachine xs [] $ do
  writeToAddress 1 a
  writeToAddress 2 b
  runMachine
  valueAtAddress 0

findNounVerb :: [Int] -> Int -> (Int, Int)
findNounVerb xs n = fst . fromJust . find ((== n) . snd) . map (\p -> (p, runProgramWithInputs xs p)) $ [(a, b) | a <- [0..99], b <- [0..99]]

printResults :: [Int] -> (String, String)
printResults xs = (part1, part2)
  where
    part1 = show $ runProgramWithInputs xs (12, 2)
    (noun, verb) = findNounVerb xs 19690720
    part2 = show $ 100 * noun + verb

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 2 >>= pure . withSuccessfulParse program printResults
