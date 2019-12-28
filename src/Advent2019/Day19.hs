module Advent2019.Day19
  ( solve
  , pulledByTractorBeam
  , nearPoints
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode (TapeSymbol, Machine)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (evalMachine, runMachine)
import Advent2019.Intcode.Machine (newMachine)

pulledByTractorBeam :: ([TapeSymbol] -> Machine) -> (Int, Int) -> Bool
pulledByTractorBeam machineFactory (x, y) = (1 ==) . head . snd . evalMachine runMachine $ machine
  where
    machine :: Machine
    machine = machineFactory [fromIntegral x, fromIntegral y]

norm :: (Floating a1, Integral a2, Integral a3) => (a2, a3) -> a1
norm (x0, y0) = sqrt $ fromIntegral x0 ^ 2 + fromIntegral y0 ^ 2

nearPoints :: (Int, Int) -> [(Int, Int)]
nearPoints (x0, y0) = search Set.empty $ Set.singleton (0, (x0, y0))
  where
    distance (x1, y1) = sqrt $ (fromIntegral x1 - fromIntegral x0) ^ 2 + (fromIntegral y1 - fromIntegral y0) ^ 2
    search visited queue = if notYetVisited then minCoord : morePoints else morePoints
      where
        minItem = Set.findMin queue
        (_, minCoord) = minItem
        (x, y) = minCoord
        notYetVisited = not $ minCoord `Set.member` visited
        neighbors = Set.fromList $ map (\p -> (distance p, p)) [(x + 1, y), (x, y + 1)]
        newQueue = Set.union neighbors (Set.delete minItem queue)
        morePoints = search (Set.insert minCoord visited) newQueue

affectedPoints :: ([TapeSymbol] -> Machine) -> [(Int, Int)]
affectedPoints machineFactory = (0, 0) : search Set.empty (0, 0)
  where
    search visited p = (nearestNeighbor : friends) ++ search newVisited nearestNeighbor
      where
        nearestNeighbor = head . filter (\n -> not (n `Set.member` visited) && pulledByTractorBeam machineFactory n) . tail . nearPoints $ p
        (x, y) = nearestNeighbor
        friends = takeWhile (pulledByTractorBeam machineFactory) [(x1, y) | x1 <- [x + 1..]]
        newVisited = Set.union visited . Set.fromList $ nearestNeighbor : friends

santasShip :: ([TapeSymbol] -> Machine) -> (Int, Int)
santasShip machineFactory = search (affectedPoints machineFactory) Set.empty
  where
    search (p:ps) visited = if bottomRightOfShip
                            then topLeftOfShip
                            else search ps $ Set.insert p visited
      where
        (x, y) = p
        seen p0 = Set.member p0 visited
        bottomRightOfShip = seen (x - 99, y) && seen (x, y - 99)
        topLeftOfShip = (x - 99, y - 99)

printResults :: [TapeSymbol] -> (String, String)
printResults program = (part1, part2)
  where
    machineFactory = newMachine program
    closePoints = filter (\(x, y) -> x < 50 && y < 50) . takeWhile (\p -> norm p <= norm (49, 49)) . affectedPoints $ machineFactory
    part1 = show . length $ closePoints
    (x, y) = santasShip machineFactory
    part2 = show $ x * 10000 + y

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 19
