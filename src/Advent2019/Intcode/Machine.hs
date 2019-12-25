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
import qualified Data.HashMap.Strict as Map

import Advent2019.Intcode (TapeSymbol, IntcodeCompute, instructionPointer, Machine(..), MachineState(..))

newMachine :: [TapeSymbol] -> [TapeSymbol] -> Machine
newMachine program input = Machine { instructionPointer = 0
                                   , relativeBase = 0
                                   , memory = tape
                                   , input = input
                                   , state = Running
                                   }
  where
    tape = Map.fromList . zip [0..] $ program

updateMemory :: [(TapeSymbol, TapeSymbol)] -> IntcodeCompute ()
updateMemory updates = do
  currentMemory <- gets memory
  let newMemory = foldr (uncurry Map.insert) currentMemory updates
  modify (\x -> x { memory = newMemory })

writeToAddress :: TapeSymbol -> TapeSymbol -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: TapeSymbol -> IntcodeCompute TapeSymbol
valueAtAddress addr = Map.lookupDefault 0 addr <$> gets memory

readInput :: IntcodeCompute TapeSymbol
readInput = (head <$> gets input) <* modify (\x -> x { input = tail . input $ x })

readInstructionPointer :: IntcodeCompute TapeSymbol
readInstructionPointer = gets instructionPointer

updateInstructionPointer :: (TapeSymbol -> TapeSymbol) -> IntcodeCompute ()
updateInstructionPointer f = modify (\x -> x { instructionPointer = f $ instructionPointer x })

getStatus :: IntcodeCompute MachineState
getStatus = gets state

setTerminated :: IntcodeCompute ()
setTerminated =  modify (\m -> m { state = Terminated })

getRelativeBase :: IntcodeCompute TapeSymbol
getRelativeBase = gets relativeBase

updateRelativeBase :: TapeSymbol -> IntcodeCompute ()
updateRelativeBase d = modify (\m -> m { relativeBase = relativeBase m + d })
