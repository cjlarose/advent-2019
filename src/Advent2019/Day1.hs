module Advent2019.Day1
  ( solve
  , fuelRequirementsForMass
  ) where

import Text.Parsec (many1, sepEndBy1, eof)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

moduleMass :: Parser Int
moduleMass = read <$> many1 digit

modules :: Parser [Int]
modules = sepEndBy1 moduleMass endOfLine <* eof

requiredFuel :: Int -> Int
requiredFuel mass = (mass `div` 3) - 2

fuelRequirementsForMass :: Int -> Int
fuelRequirementsForMass mass = fuelForMass + if fuelForMass > 0
                                             then fuelRequirementsForMass fuelForMass
                                             else 0
  where
    fuelForMass = max 0 $ requiredFuel mass

printResults :: [Int] -> (String, String)
printResults xs = (part1, part2)
  where
    fuelForModules = sum . map requiredFuel $ xs
    part1 = show fuelForModules
    totalFuel = sum . map fuelRequirementsForMass $ xs
    part2 = show totalFuel

solve :: IO (Either String (String, String))
solve = withSuccessfulParse modules printResults <$> getProblemInputAsByteString 1
