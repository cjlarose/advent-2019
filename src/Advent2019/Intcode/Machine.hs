module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , emitOutput
  , readInstructionPointer
  , updateInstructionPointer
  , newMachine
  , getStatus
  , setTerminated
  , getRelativeBase
  , updateRelativeBase
  ) where

import Control.Monad.State (gets, put, modify)
import Control.Monad.Writer (tell)
import Data.Vector.Unboxed (Vector, fromList, (//), (!))
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

updateMemory :: (Vector TapeSymbol -> Vector TapeSymbol) -> IntcodeCompute ()
updateMemory f = do
  mem <- gets memory
  modify (\x -> x { memory = f mem })

writeToAddress :: TapeSymbol -> TapeSymbol -> IntcodeCompute ()
writeToAddress addr val = updateMemory go
  where
    go mem | fromIntegral addr < Vector.length mem = Vector.unsafeUpd mem [(fromIntegral addr, val)]
           | otherwise = grownMemory // [(fromIntegral addr, val)]
      where
        oldLength = Vector.length mem
        newLength = fromIntegral addr + 1
        diffLength = newLength - oldLength
        grownMemory = mem Vector.++ Vector.replicate diffLength 0

valueAtAddress :: TapeSymbol -> IntcodeCompute TapeSymbol
valueAtAddress addr = go <$> gets memory
  where
    go mem | fromIntegral addr < Vector.length mem = Vector.unsafeIndex mem $ fromIntegral addr
           | otherwise = 0

readInput :: IntcodeCompute TapeSymbol
readInput = (head <$> gets input) <* modify (\x -> x { input = tail . input $ x })

emitOutput :: TapeSymbol -> IntcodeCompute ()
emitOutput = tell . pure

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
