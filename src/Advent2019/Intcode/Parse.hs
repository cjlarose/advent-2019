module Advent2019.Intcode.Parse
  ( program
  ) where

import Text.Parsec (many1, sepBy1, eof, option)
import Text.Parsec.Char (endOfLine, digit, char)
import Text.Parsec.ByteString (Parser)
import Advent2019.Intcode (TapeSymbol)

program :: Parser [TapeSymbol]
program = sepBy1 instruction (char ',') <* endOfLine <* eof
  where
    instruction = (\a b -> read $ a ++ b) <$> (option "" (pure <$> char '-')) <*> many1 digit
