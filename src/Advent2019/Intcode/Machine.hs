module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , updateInstructionPointer
  ) where

import Control.Monad.State (get, put, modify)
import Data.Array.Unboxed ((//), (!))

import Advent2019.Intcode (IntcodeCompute, instructionPointer, Machine(..))

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

updateInstructionPointer :: (Integer -> Integer) -> IntcodeCompute ()
updateInstructionPointer f = modify $ (\x -> x { instructionPointer = f $ instructionPointer x })
