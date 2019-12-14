module Advent2019.Day14
  ( solve
  ) where

import Text.Parsec (many1, sepEndBy1, eof, sepBy1)
import Text.Parsec.Char (endOfLine, digit, space, char, upper)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

data Reaction = Reaction { reactants :: [(Int, String)]
                         , product :: (Int, String)
                         } deriving Show

reactions :: Parser [Reaction]
reactions = sepEndBy1 reaction endOfLine <* eof
  where
    quantityChemical = (\a b -> (a, b)) <$> (read <$> many1 digit <* many1 space) <*> many1 upper
    lhs = sepBy1 quantityChemical (char ',' >> many1 space)
    reaction = Reaction <$> (lhs <* many1 space <* char '=' <* char '>' <* many1 space) <*> quantityChemical

printResults :: [Reaction] -> (String, String)
printResults xs = (part1, part2)
  where
    part1 = show xs
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 14 >>= pure . withSuccessfulParse reactions printResults
