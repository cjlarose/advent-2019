module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  , updateInstructionPointer
  ) where

import Control.Monad.State (get, put)
import Data.Array.Unboxed ((//), (!))

import Advent2019.Intcode (IntcodeCompute)

updateMemory :: [(Int, Int)] -> IntcodeCompute ()
updateMemory updates = do
  (pc, memory, input, status) <- get
  let newMemory = memory // updates
  put (pc, newMemory, input, status)

writeToAddress :: Int -> Int -> IntcodeCompute ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Int -> IntcodeCompute Int
valueAtAddress addr = do
  (_, memory, _, _) <- get
  pure $ memory ! addr

readInput :: IntcodeCompute Int
readInput = do
  (pc, memory, input, status) <- get
  let val = head input
  put (pc, memory, tail input, status)
  pure val

updateInstructionPointer :: (Int -> Int) -> IntcodeCompute ()
updateInstructionPointer f = do
  (pc, memory, input, status) <- get
  put (f pc, memory, input, status)
