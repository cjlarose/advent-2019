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
import Data.Vector.Unboxed (fromList, (//), (!))
import qualified Data.Vector.Unboxed as Vector

import Advent2019.Intcode (TapeSymbol, IntcodeCompute, instructionPointer, Machine(..), MachineState(..))

newMachine :: [TapeSymbol] -> [TapeSymbol] -> Machine
newMachine program input = Machine { instructionPointer = 0
                                   , relativeBase = 0
                                   , memory = tape
                                   , input = input
                                   , state = Running
                                   }
  where
    tape = fromList program

writeToAddress :: TapeSymbol -> TapeSymbol -> IntcodeCompute ()
writeToAddress addr val = do
  mem <- gets memory
  let oldLength = Vector.length mem
  let newLength = fromIntegral addr + 1
  let diffLength = newLength - oldLength
  let grownMemory = mem Vector.++ Vector.replicate diffLength 0
  let newMem = grownMemory // [(fromIntegral addr, val)]
  modify (\x -> x { memory = newMem })

valueAtAddress :: TapeSymbol -> IntcodeCompute TapeSymbol
valueAtAddress addr = go <$> gets memory
  where
    go mem | fromIntegral addr < Vector.length mem = mem ! fromIntegral addr
           | otherwise = 0

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
