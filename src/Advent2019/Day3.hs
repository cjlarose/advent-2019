module Advent2019.Day3
  ( solve
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (minimumBy, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

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

intersectingPoints :: [(Int, Int)] -> [(Int, Int)] -> Set (Int, Int)
intersectingPoints p0 p1 = Set.delete (0,0) $ Set.intersection (coords p0) (coords p1)
  where
    coords = Set.fromList

oneNorm :: (Int, Int) -> Int
oneNorm (x, y) = abs x + abs y

stepsTakenToArriveAtPoint :: (Int, Int) -> [(Int, Int)] -> Int
stepsTakenToArriveAtPoint dest path = fromJust $ elemIndex dest path

combinedSteps :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
combinedSteps p0 p1 x = stepsTakenToArriveAtPoint x p0
                      + stepsTakenToArriveAtPoint x p1

bestIntersection :: [(Int, Int)] -> [(Int, Int)] -> Set.Set (Int, Int) -> (Int, Int)
bestIntersection p0 p1 intersecting = minimumBy (comparing $ combinedSteps p0 p1) intersecting

printResults :: (WirePath, WirePath) -> (String, String)
printResults (p0, p1) = (part1, part2)
  where
    path0 = wireCoords p0
    path1 = wireCoords p1
    intersecting = intersectingPoints path0 path1
    closestPoint = minimumBy (comparing oneNorm) . Set.toList $ intersecting
    part1 = show . oneNorm $ closestPoint
    best = bestIntersection path0 path1 intersecting
    part2 = show . combinedSteps path0 path1 $ best

solve :: IO (Either String (String, String))
solve = withSuccessfulParse wires printResults <$> getProblemInputAsByteString 3
