module Advent2019.Day9
  ( solve
  ) where

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode (TapeSymbol)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

testMode = 1
sensorBoostMode = 2

runProgramInMode :: TapeSymbol -> [TapeSymbol] -> [TapeSymbol]
runProgramInMode mode program = runMachineWithInput program [mode]

printResults :: [TapeSymbol] -> (String, String)
printResults program = (part1, part2)
  where
    boostKeycode = last . runProgramInMode testMode $ program
    part1 = show boostKeycode
    distressSignalCoordinates = last . runProgramInMode sensorBoostMode $ program
    part2 = show distressSignalCoordinates

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 9
