module Advent2019.Day1 (
  solve
  ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Text.Parsec (ParseError, many1, sepEndBy1)
import qualified Text.Parsec (parse)
import Text.Parsec.Char (endOfLine, digit)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString)

moduleMass :: Parser Int
moduleMass = read <$> many1 digit

modules :: Parser [Int]
modules = sepEndBy1 moduleMass endOfLine

handleParseError :: ParseError -> IO ()
handleParseError err = do
  hPutStrLn stderr "Parse error:"
  hPutStrLn stderr . show $ err
  exitFailure

requiredFuel :: Int -> Int
requiredFuel mass = (mass `div` 3) - 2

part1 :: [Int] -> Int
part1 = sum . map requiredFuel

solve :: IO ()
solve = do
  input <- getProblemInputAsByteString 1
  let parseResult = Text.Parsec.parse modules "" input
  either handleParseError (putStrLn . show . part1) parseResult
