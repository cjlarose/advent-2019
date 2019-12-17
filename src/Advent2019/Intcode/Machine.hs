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

import Control.Monad.State (gets, put, modify)
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
  currentMemory <- gets memory
  let newMemory = foldr (uncurry Map.insert) currentMemory updates
  modify (\x -> x { memory = newMemory })

writeToAddress :: Integer -> Integer -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Integer -> IntcodeCompute Integer
valueAtAddress addr = maybe 0 id . Map.lookup addr <$> gets memory

readInput :: IntcodeCompute Integer
readInput = (head <$> gets input) <* modify (\x -> x { input = tail . input $ x })

readInstructionPointer :: IntcodeCompute Integer
readInstructionPointer = gets instructionPointer

updateInstructionPointer :: (Integer -> Integer) -> IntcodeCompute ()
updateInstructionPointer f = modify (\x -> x { instructionPointer = f $ instructionPointer x })

getStatus :: IntcodeCompute MachineState
getStatus = gets state

setTerminated :: IntcodeCompute ()
setTerminated =  modify (\m -> m { state = Terminated })

getRelativeBase :: IntcodeCompute Integer
getRelativeBase = gets relativeBase

updateRelativeBase :: Integer -> IntcodeCompute ()
updateRelativeBase d = modify (\m -> m { relativeBase = relativeBase m + d })
