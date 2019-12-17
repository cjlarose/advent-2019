module Advent2019.Day9
  ( solve
  ) where

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

runProgramInTestMode :: [Integer] -> [Integer]
runProgramInTestMode program = runMachineWithInput program $ [1]

printResults :: [Integer] -> (String, String)
printResults program = (part1, part2)
  where
    boostKeycode = last . runProgramInTestMode $ program
    part1 = show boostKeycode
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 9 >>= pure . withSuccessfulParse program printResults