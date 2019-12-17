module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , readInstructionPointer
  , updateInstructionPointer
  , newMachine
  , getStatus
  ) where

import Control.Monad.State (get, put, modify)
import Data.Array.Unboxed ((//), (!), listArray)

import Advent2019.Intcode (IntcodeCompute, instructionPointer, Machine(..), MachineState(..))

newMachine :: [Integer] -> [Integer] -> Machine
newMachine program input = Machine 0 arr input Running
  where
    n = fromIntegral . length $ program
    arr = listArray (0, n - 1) program

updateMemory :: [(Integer, Integer)] -> IntcodeCompute ()
updateMemory updates = do
  newMemory <- (\x -> x // updates) . memory <$> get
  modify $ (\x -> x { memory = newMemory })

writeToAddress :: Integer -> Integer -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Integer -> IntcodeCompute Integer
valueAtAddress addr = (\x -> x ! addr) . memory <$> get

readInput :: IntcodeCompute Integer
readInput = (head . input <$> get) <* (modify $ (\x -> x { input = tail . input $ x }))

readInstructionPointer :: IntcodeCompute Integer
readInstructionPointer = instructionPointer <$> get

updateInstructionPointer :: (Integer -> Integer) -> IntcodeCompute ()
updateInstructionPointer f = modify $ (\x -> x { instructionPointer = f $ instructionPointer x })

getStatus :: IntcodeCompute MachineState
getStatus = state <$> get
