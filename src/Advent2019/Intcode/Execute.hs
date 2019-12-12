module Advent2019.Intcode.Execute
  ( withMachine
  , valueAtAddress
  , writeToAddress
  , runMachine
  ) where

import Data.Array.IArray (IArray, listArray, (!), (//))
import Data.Array.Unboxed (UArray)
import Control.Monad.State (State, get, put, evalState)

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, MachineState)

newMachine :: [Int] -> Machine
newMachine xs = (0, arr, Running)
  where
    n = length xs
    arr = listArray (0, n - 1) xs

updateMemory :: [(Int, Int)] -> State Machine ()
updateMemory updates = do
  (pc, memory, status) <- get
  let newMemory = memory // updates
  put (pc, newMemory, status)

writeToAddress :: Int -> Int -> State Machine ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Int -> State Machine Int
valueAtAddress addr = do
  (_, memory, _) <- get
  pure $ memory ! addr

dereference :: UArray Int Int -> Int -> Int
dereference arr i = arr ! address
  where address = arr ! i

executeBinaryOp :: (Int -> Int -> Int) -> UArray Int Int -> Int -> UArray Int Int
executeBinaryOp f memory pc = memory // [(dest, res)]
  where
    operand1 = dereference memory $ pc + 1
    operand2 = dereference memory $ pc + 2
    dest = memory ! (pc + 3)
    res = operand1 `f` operand2

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, memory, _) <- get
  let opcode = memory ! pc
  case opcode of
    1 -> do
      let newMemory = executeBinaryOp (+) memory pc
      put (pc + 4, newMemory, Running)
    2 -> do
      let newMemory = executeBinaryOp (*) memory pc
      put (pc + 4, newMemory, Running)
    99 -> put (pc, memory, Terminated)

runMachine :: State Machine ()
runMachine = do
  (_, memory, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> State Machine a -> a
withMachine program action = evalState action (newMachine program)
