module Advent2019.Day9
  ( solve
  ) where

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

testMode = 1
sensorBoostMode = 2

runProgramInMode :: Integer -> [Integer] -> [Integer]
runProgramInMode mode program = runMachineWithInput program [mode]

printResults :: [Integer] -> (String, String)
printResults program = (part1, part2)
  where
    boostKeycode = last . runProgramInMode 1 $ program
    part1 = show boostKeycode
    distressSignalCoordinates = last . runProgramInMode 2 $ program
    part2 = show distressSignalCoordinates

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 9 >>= pure . withSuccessfulParse program printResults
