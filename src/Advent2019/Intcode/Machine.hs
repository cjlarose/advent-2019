module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , updateInstructionPointer
  ) where

import Control.Monad.State (get, put, modify)
import Data.Array.Unboxed ((//), (!))

import Advent2019.Intcode (IntcodeCompute, instructionPointer, Machine(..))

updateMemory :: [(Int, Int)] -> IntcodeCompute ()
updateMemory updates = do
  newMemory <- (\x -> x // updates) . memory <$> get
  modify $ (\x -> x { memory = newMemory })

writeToAddress :: Int -> Int -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Int -> IntcodeCompute Int
valueAtAddress addr = (\x -> x ! addr) . memory <$> get

readInput :: IntcodeCompute Int
readInput = (head . input <$> get) <* (modify $ (\x -> x { input = tail . input $ x }))

updateInstructionPointer :: (Int -> Int) -> IntcodeCompute ()
updateInstructionPointer f = modify $ (\x -> x { instructionPointer = f $ instructionPointer x })
