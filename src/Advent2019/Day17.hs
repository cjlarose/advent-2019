module Advent2019.Day17
  ( solve
  ) where

import qualified Data.Set as Set

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode (TapeSymbol)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

data Scene = Scene { n :: Int
                   , m :: Int
                   , scaffold :: Set.Set (Int, Int) } deriving Show

parseAsciiOutput :: [TapeSymbol] -> String
parseAsciiOutput = map (toEnum . fromIntegral)

parseScaffold :: [String] -> Set.Set (Int, Int)
parseScaffold = Set.unions . zipWith parseRow [0..]
  where
    parseRow :: Int -> String -> Set.Set (Int, Int)
    parseRow i = foldr (\(j, col) acc -> if col == '#'
                                         then Set.insert (i, j) acc
                                         else acc) Set.empty . zip [0..]

newScene :: String -> Scene
newScene ascii = Scene n m scaffold
  where
    rows = filter (not . null) . lines $ ascii
    n = length rows
    m = length . head $ rows
    scaffold = parseScaffold rows

scaffoldIntersections :: Scene -> Set.Set (Int, Int)
scaffoldIntersections scene = Set.filter isIntersection . scaffold $ scene
  where
    isIntersection p = neighbors p `Set.isSubsetOf` scaffold scene
    neighbors (i, j) = Set.fromList [ (i, j + 1)
                                    , (i, j - 1)
                                    , (i + 1, j)
                                    , (i - 1, j) ]

sumOfAlignmentParameters :: Set.Set (Int, Int) -> Int
sumOfAlignmentParameters = Set.foldr (\(i, j) acc -> acc + i * j) 0

printResults :: [TapeSymbol] -> (String, String)
printResults program = (part1, part2)
  where
    programOutput = runMachineWithInput program []
    scene = newScene . parseAsciiOutput $ programOutput
    part1 = show . sumOfAlignmentParameters . scaffoldIntersections $ scene
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 17
