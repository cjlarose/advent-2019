module Advent2019.Day2
  ( solve
  ) where

import Text.Parsec (many1, sepBy1, eof)
import Text.Parsec.Char (endOfLine, digit, char)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

program :: Parser [Int]
program = sepBy1 instruction (char ',') <* endOfLine <* eof
  where
    instruction = read <$> many1 digit

printResults :: [Int] -> IO ()
printResults xs = do
  putStrLn . show $ xs

solve :: IO ()
solve = getProblemInputAsByteString 2 >>= withSuccessfulParse program printResults
