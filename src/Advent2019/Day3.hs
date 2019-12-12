module Advent2019.Day3
  ( solve
  ) where

import Text.Parsec (many1, sepBy1, eof, (<|>))
import Text.Parsec.Char (endOfLine, digit, char)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

data Direction = U | D | R | L deriving Show
type Step = (Direction, Int)
type WirePath = [Step]

wirePath :: Parser WirePath
wirePath = sepBy1 step (char ',')
  where
    dir :: Direction -> Parser Direction
    dir d = d <$ char (head $ show d)
    directionSpecification = dir U <|> dir D <|> dir R <|> dir L
    distance = read <$> many1 digit
    step :: Parser Step
    step = (\a b -> (a, b)) <$> directionSpecification <*> distance

wires :: Parser (WirePath, WirePath)
wires = (\a b -> (a, b)) <$> (wirePath <* endOfLine) <*> (wirePath <* endOfLine) <* eof

wireCoords :: WirePath -> [(Int, Int)]
wireCoords xs = (0, 0) : coords (0, 0) xs
  where
    coords :: (Int, Int) -> WirePath -> [(Int, Int)]
    coords (x, y) [] = []
    coords (x, y) ((dir, displacement):ds) = newCoords ++ coords (last newCoords) ds
        where
          newCoords = take displacement [(x1, y1) | t <- [1..], x1 <- [x + dx * t], y1 <- [y + dy * t]]
          (dx, dy) = case dir of
                       U -> ( 0,  1)
                       D -> ( 0, -1)
                       L -> (-1,  0)
                       R -> ( 1,  0)

printResults :: (WirePath, WirePath) -> IO ()
printResults (p1, p2) = do
  putStrLn . show $ p1
  putStrLn . show $ p2
  putStrLn . show . wireCoords $ [(R,8),(U,5),(L,5),(D,3)]

solve :: IO ()
solve = getProblemInputAsByteString 3 >>= withSuccessfulParse wires printResults
