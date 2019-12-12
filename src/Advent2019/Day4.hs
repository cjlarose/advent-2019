module Advent2019.Day4
  ( solve
  ) where

import Text.Parsec (many1, eof)
import Text.Parsec.Char (digit, char)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

range :: Parser (Int, Int)
range = (\min max -> (min, max)) <$> (integer <* char '-') <*> (integer <* eof)
  where integer = read <$> many1 digit

printResults :: (Int, Int) -> IO ()
printResults (min, max) = do
  print (min, max)

solve :: IO ()
solve = getProblemInputAsByteString 4 >>= withSuccessfulParse range printResults
