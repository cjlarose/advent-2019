module Advent2019.Day19
  ( solve
  ) where

import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.Set (Set)
import qualified Data.Set as Set

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (runMachineWithInput)

pulledByTractorBeam :: [Integer] -> (Integer, Integer) -> Bool
pulledByTractorBeam program (x, y) = (1 ==) . head . runMachineWithInput program $ [x, y]

norm :: (Floating a1, Integral a2, Integral a3) => (a2, a3) -> a1
norm (x0, y0) = sqrt $ fromIntegral x0 ^ 2 + fromIntegral y0 ^ 2

affectedPoints :: [Integer] -> [(Integer, Integer)]
affectedPoints program = search Set.empty $ Set.singleton (0, (0, 0))
  where
    search visited queue = if pulled
                           then minCoord : morePoints
                           else morePoints
      where
        minItem = Set.findMin queue
        (_, minCoord) = minItem
        (x, y) = minCoord
        pulled = not (minCoord `Set.member` visited) && pulledByTractorBeam program minCoord
        neighbors = Set.fromList $ map (\p -> (norm p, p)) [(x + 1, y), (x, y + 1)]
        newQueue = Set.union neighbors (Set.delete minItem queue)
        morePoints = search (Set.insert minCoord visited) newQueue

pointsByDistance :: [(Integer, Integer)]
pointsByDistance = f 0
  where
    f d = [(x, y) | x <- [0..d], let y = d - x] ++ f (d + 1)

santasShip :: [Integer] -> (Integer, Integer)
santasShip program = findInBeam pointsByDistance Map.empty
  where
    findInBeam (p:ps) cache = if newCache ! p
                              then (if canHoldSantasShip
                                    then p
                                    else findInBeam ps newNewCache)
                              else findInBeam ps newCache
      where
        queryBeam :: Map.Map (Integer, Integer) Bool -> (Integer, Integer) -> Map.Map (Integer, Integer) Bool
        queryBeam m p = if p `Map.member` m
                        then m
                        else Map.insert p (pulledByTractorBeam program p) m

        newCache = queryBeam cache p
        (x0, y0) = p
        neighbors = [(x, y) | x <- [x0..x0+99], y <- [y0..y0+99]]
        newNewCache = foldr (flip queryBeam) newCache neighbors
        canHoldSantasShip = all (newNewCache !) neighbors

printResults :: [Integer] -> (String, String)
printResults program = (part1, part2)
  where
    numPoints = length . filter (\(x, y) -> x < 50 && y < 50) . takeWhile (\p -> norm p <= norm (49, 49)) . affectedPoints $ program
    part1 = show numPoints
    -- (x, y) = santasShip program
    -- part2 = show $ x * 10000 + y
    part2 = "not yet implemented"

solve :: IO (Either String (String, String))
solve = withSuccessfulParse program printResults <$> getProblemInputAsByteString 19
