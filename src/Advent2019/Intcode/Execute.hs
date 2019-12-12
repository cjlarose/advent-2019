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

dereference :: Int -> State Machine Int
dereference idx = valueAtAddress idx >>= valueAtAddress

executeBinaryOp :: (Int -> Int -> Int) -> State Machine ()
executeBinaryOp f = do
  (pc, _, _) <- get
  operand1 <- dereference $ pc + 1
  operand2 <- dereference $ pc + 2
  destAddr <- valueAtAddress $ pc + 3
  let res = operand1 `f` operand2
  writeToAddress destAddr res

updateInstructionPointer :: (Int -> Int) -> State Machine ()
updateInstructionPointer f = do
  (pc, memory, status) <- get
  put (f pc, memory, status)

halt :: State Machine ()
halt = do
  (ip, memory, status) <- get
  put (ip, memory, Terminated)

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, memory, _) <- get
  let opcode = memory ! pc
  case opcode of
    1 -> executeBinaryOp (+) >> updateInstructionPointer (+ 4)
    2 -> executeBinaryOp (*) >> updateInstructionPointer (+ 4)
    99 -> halt

runMachine :: State Machine ()
runMachine = do
  (_, memory, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> State Machine a -> a
withMachine program action = evalState action (newMachine program)
