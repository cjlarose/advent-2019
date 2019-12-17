module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , readInstructionPointer
  , updateInstructionPointer
  , newMachine
  , getStatus
  , setTerminated
  , getRelativeBase
  , updateRelativeBase
  ) where

import Control.Monad.State (get, put, modify)
import qualified Data.Map.Strict as Map

import Advent2019.Intcode (IntcodeCompute, instructionPointer, Machine(..), MachineState(..))

newMachine :: [Integer] -> [Integer] -> Machine
newMachine program input = Machine { instructionPointer = 0
                                   , relativeBase = 0
                                   , memory = tape
                                   , input = input
                                   , state = Running
                                   }
  where
    tape = Map.fromList . zip [0..] $ program

updateMemory :: [(Integer, Integer)] -> IntcodeCompute ()
updateMemory updates = do
  currentMemory <- memory <$> get
  let newMemory = foldr (uncurry Map.insert) currentMemory updates
  modify $ (\x -> x { memory = newMemory })

writeToAddress :: Integer -> Integer -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Integer -> IntcodeCompute Integer
valueAtAddress addr = maybe 0 id . Map.lookup addr . memory <$> get

readInput :: IntcodeCompute Integer
readInput = (head . input <$> get) <* (modify $ (\x -> x { input = tail . input $ x }))

readInstructionPointer :: IntcodeCompute Integer
readInstructionPointer = instructionPointer <$> get

updateInstructionPointer :: (Integer -> Integer) -> IntcodeCompute ()
updateInstructionPointer f = modify $ (\x -> x { instructionPointer = f $ instructionPointer x })

getStatus :: IntcodeCompute MachineState
getStatus = state <$> get

setTerminated :: IntcodeCompute ()
setTerminated =  modify $ (\m -> m { state = Terminated })

getRelativeBase :: IntcodeCompute Integer
getRelativeBase = relativeBase <$> get

updateRelativeBase :: Integer -> IntcodeCompute ()
updateRelativeBase d = modify $ (\m -> m { relativeBase = relativeBase m + d })
