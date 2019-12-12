module Advent2019.Intcode.Parse
  ( program
  ) where

import Text.Parsec (many1, sepBy1, eof)
import Text.Parsec.Char (endOfLine, digit, char)
import Text.Parsec.ByteString (Parser)

program :: Parser [Int]
program = sepBy1 instruction (char ',') <* endOfLine <* eof
  where
    instruction = read <$> many1 digit
