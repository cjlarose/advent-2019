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

executeBinaryOp :: (Int -> Int -> Int) -> [Int] -> State Machine ()
executeBinaryOp f [a0, a1, destAddr] = do
  operand1 <- valueAtAddress a0
  operand2 <- valueAtAddress a1
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

instruction :: Int -> ([Int] -> State Machine a) -> State Machine ()
instruction numParams effect = do
  (pc, memory, _) <- get
  let operands = map (\p -> memory ! (pc + p + 1)) [0..numParams-1]
  effect operands
  updateInstructionPointer (+ (numParams + 1))

addInstruction :: State Machine ()
addInstruction = instruction 3 $ executeBinaryOp (+)

multiplyInstruction :: State Machine ()
multiplyInstruction = instruction 3 $ executeBinaryOp (*)

haltInstruction :: State Machine ()
haltInstruction = instruction 0 $ const halt

decodeAndExecute :: Int -> State Machine ()
decodeAndExecute 1 = addInstruction
decodeAndExecute 2 = multiplyInstruction
decodeAndExecute 99 = haltInstruction

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, memory, _) <- get
  let opcode = memory ! pc
  decodeAndExecute opcode

runMachine :: State Machine ()
runMachine = do
  (_, memory, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> State Machine a -> a
withMachine program action = evalState action (newMachine program)
