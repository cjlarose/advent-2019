module Advent2019.Day17
  ( solve
  ) where

import Data.List (find)
import qualified Data.Set as Set

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode (TapeSymbol)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

data Vacuum = Vacuum { position :: (Int, Int)
                      , direction :: (Int, Int) } deriving Show

data Scene = Scene { n :: Int
                   , m :: Int
                   , scaffold :: Set.Set (Int, Int)
                   , vacuum :: Vacuum } deriving Show

parseAsciiOutput :: [TapeSymbol] -> String
parseAsciiOutput = map (toEnum . fromIntegral)

parseChars :: [String] -> [((Int, Int), Char)]
parseChars = concat . zipWith parseRow [0..]
  where
    parseRow :: Int -> String -> [((Int, Int), Char)]
    parseRow i = zipWith (\j col -> ((i, j), col)) [0..]

parseScaffold :: [((Int, Int), Char)] -> Set.Set (Int, Int)
parseScaffold = Set.fromList . map fst . filter (\(_, c) -> c == '#')

parseVacuum :: [((Int, Int), Char)] -> Vacuum
parseVacuum xs = Vacuum position direction
  where
    isVaccum (_, c) = c == '^' || c == '>' || c == 'v' || c == '<'
    Just (position, vacChar) = find isVaccum xs
    direction = case vacChar of
                  '^' -> (-1, 0)
                  '>' -> (0, 1)
                  'v' -> (1, 0)
                  '<' -> (0, -1)

newScene :: String -> Scene
newScene ascii = Scene n m scaffold vacuum
  where
    rows = filter (not . null) . lines $ ascii
    n = length rows
    m = length . head $ rows
    content = parseChars rows
    scaffold = parseScaffold content
    vacuum = parseVacuum content

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
