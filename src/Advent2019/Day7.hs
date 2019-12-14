module Advent2019.Day7
  ( solve
  , highestSignal
  , trySettings
  ) where

import Data.List (permutations)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Execute (withMachine, runMachine)

runMachineWithInput :: [Int] -> [Int] -> [Int]
runMachineWithInput xs input = snd $ withMachine xs input runMachine

type PhaseSettings = (Int, Int, Int, Int, Int)

trySettings :: [Int] -> PhaseSettings -> Int
trySettings xs (a,b,c,d,e) = last outputE
  where
    amp = runMachineWithInput xs
    outputA = amp [a, 0]
    outputB = amp (b : outputA)
    outputC = amp (c : outputB)
    outputD = amp (d : outputC)
    outputE = amp (e : outputD)

highestSignal :: [Int] -> (Int)
highestSignal xs = maximum $ map (trySettings xs) possibleSettings
  where
    possibleSettings = [(a, b, c, d, e) | (a:b:c:d:e:[]) <- permutations [0..4]]

trySettingsWithFeedback :: [Int] -> PhaseSettings -> Int
trySettingsWithFeedback xs (a,b,c,d,e) = last outputE
  where
    amp = runMachineWithInput xs
    outputA = amp (a : 0 : outputE)
    outputB = amp (b : outputA)
    outputC = amp (c : outputB)
    outputD = amp (d : outputC)
    outputE = amp (e : outputD)

highestSignalWithFeedback :: [Int] -> (Int)
highestSignalWithFeedback xs = maximum $ map (trySettingsWithFeedback xs) possibleSettings
  where
    possibleSettings = [(a, b, c, d, e) | (a:b:c:d:e:[]) <- permutations [5..9]]

printResults :: [Int] -> (String, String)
printResults xs = (part1, part2)
  where
    signal = highestSignal xs
    signalWithFeedback = highestSignalWithFeedback xs
    part1 = show signal
    part2 = show signalWithFeedback

solve :: IO (Either String (String, String))
solve = getProblemInputAsByteString 7 >>= pure . withSuccessfulParse program printResults
