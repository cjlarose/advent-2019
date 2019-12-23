module Advent2019.Day19
  ( solve
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.RWS (evalRWS)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput, runMachine)
import Advent2019.Intcode.Machine (newMachine)
import Advent2019.Intcode (Machine)

pulledByTractorBeam :: ([Integer] -> Machine) -> (Integer, Integer) -> Bool
pulledByTractorBeam machineFactory (x, y) = (1 ==) . head . snd . evalRWS runMachine () $ machine
  where
    machine :: Machine
    machine = machineFactory [x, y]

norm :: (Floating a1, Integral a2, Integral a3) => (a2, a3) -> a1
norm (x0, y0) = sqrt $ fromIntegral x0 ^ 2 + fromIntegral y0 ^ 2

affectedPoints :: ([Integer] -> Machine) -> [(Integer, Integer)]
affectedPoints machineFactory = search Set.empty $ Set.singleton (0, (0, 0))
  where
    search visited queue = if pulled
                           then minCoord : morePoints
                           else morePoints
      where
        minItem = Set.findMin queue
        (_, minCoord) = minItem
        (x, y) = minCoord
        pulled = not (minCoord `Set.member` visited) && pulledByTractorBeam machineFactory minCoord
        neighbors = Set.fromList $ map (\p -> (norm p, p)) [(x + 1, y), (x, y + 1)]
        newQueue = Set.union neighbors (Set.delete minItem queue)
        morePoints = search (Set.insert minCoord visited) newQueue

santasShip :: ([Integer] -> Machine) -> (Integer, Integer)
santasShip machineFactory = search (affectedPoints machineFactory) Set.empty
  where
    search (p:ps) visited = if bottomRightOfShip
                            then topLeftOfShip
                            else search ps $ Set.insert p visited
      where
        (x, y) = p
        seen p0 = Set.member p0 visited
        bottomRightOfShip = seen (x - 50, y) && seen (x, y - 50)
        topLeftOfShip = (x - 50, y - 50)

printResults :: [Integer] -> (String, String)
printResults program = (part1, part2)
  where
    machineFactory = newMachine program
    numPoints = length . filter (\(x, y) -> x < 50 && y < 50) . takeWhile (\p -> norm p <= norm (49, 49)) . affectedPoints $ machineFactory
    part1 = show numPoints
    -- (x, y) = santasShip machineFactory
    -- part2 = show $ x * 10000 + y
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 19
