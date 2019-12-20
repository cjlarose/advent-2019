module Advent2019.Day19
  ( solve
  ) where

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

pulledByTractorBeam :: [Integer] -> (Integer, Integer) -> Bool
pulledByTractorBeam program (x, y) = (1 ==) . head . runMachineWithInput program $ [x, y]

affectedPoints :: [Integer] -> [(Integer, Integer)]
affectedPoints program = filter (pulledByTractorBeam program) pointsToTest
  where
    pointsToTest = [(x, y) | x <- [0..49], y <- [0..49]]

printResults :: [Integer] -> (String, String)
printResults program = (part1, part2)
  where
    numPoints = length . affectedPoints $ program
    part1 = show numPoints
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 19
