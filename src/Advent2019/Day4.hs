module Advent2019.Day4
  ( solve
  ) where

import Data.List (find, group)
import Data.Maybe (isJust)

import Text.Parsec (many1, eof)
import Text.Parsec.Char (digit, char)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

range :: Parser (Int, Int)
range = (\min max -> (min, max)) <$> (integer <* char '-') <*> (integer <* eof)
  where integer = read <$> many1 digit

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

hasRepeatedDigitPair :: Int -> Bool
hasRepeatedDigitPair x = isJust . find (\(a, b) -> a == b) . pairs . show $ x

monotonicallyIncreasing :: Ord a => [a] -> Bool
monotonicallyIncreasing [] = True
monotonicallyIncreasing [x] = True
monotonicallyIncreasing (a:b:rest) = a <= b && monotonicallyIncreasing (b:rest)

hasMonotonicallyIncreasingDigits :: Int -> Bool
hasMonotonicallyIncreasingDigits = monotonicallyIncreasing . show

isCandidate :: Int -> Bool
isCandidate x = hasMonotonicallyIncreasingDigits x && hasRepeatedDigitPair x

hasRepeatedDigitPairThatIsntPartOfATriple :: Int -> Bool
hasRepeatedDigitPairThatIsntPartOfATriple x = isJust . find (\g -> length g == 2) . group . show $ x

printResults :: (Int, Int) -> (String, String)
printResults (min, max) = (part1, part2)
  where
    candidates = filter isCandidate $ [min..max]
    part1 = show . length $ candidates
    numPotentialPasswords = length . filter hasRepeatedDigitPairThatIsntPartOfATriple $ candidates
    part2 = show $ numPotentialPasswords

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 4 >>= pure . withSuccessfulParse range printResults
