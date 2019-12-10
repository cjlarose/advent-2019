module Advent2019.Day1
  ( solve
  , fuelRequirementsForMass
  ) where

import Text.Parsec (ParseError, many1, sepEndBy1)
import qualified Text.Parsec (parse)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

moduleMass :: Parser Int
moduleMass = read <$> many1 digit

modules :: Parser [Int]
modules = sepEndBy1 moduleMass endOfLine

requiredFuel :: Int -> Int
requiredFuel mass = (mass `div` 3) - 2

fuelRequirementsForMass :: Int -> Int
fuelRequirementsForMass mass = fuelForMass + if fuelForMass > 0
                                             then fuelRequirementsForMass fuelForMass
                                             else 0
  where
    fuelForMass = max 0 $ requiredFuel mass

printResults :: [Int] -> IO ()
printResults xs = do
  let fuelForModules = sum . map requiredFuel $ xs
  putStrLn . show $ fuelForModules
  let totalFuel = sum . map fuelRequirementsForMass $ xs
  putStrLn . show $ totalFuel

solve :: IO ()
solve = getProblemInputAsByteString 1 >>= withSuccessfulParse modules printResults
