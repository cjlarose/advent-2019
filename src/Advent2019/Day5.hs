module Advent2019.Day5
  ( solve
  ) where

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Machine (writeToAddress, valueAtAddress)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (withMachine, runMachine)

runMachineWithInput :: [Int] -> [Int] -> [Int]
runMachineWithInput xs input = snd $ withMachine xs input runMachine

printResults :: [Int] -> (String, String)
printResults xs = (part1, part2)
  where
    airConditionerTestOutput = runMachineWithInput xs [1]
    part1 = show . last $ airConditionerTestOutput
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 5 >>= pure . withSuccessfulParse program printResults
