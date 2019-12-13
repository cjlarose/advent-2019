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

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int

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

executeBinaryOp :: (Int -> Int -> Int) -> [Operand] -> State Machine ()
executeBinaryOp f [a1, a2, Position destAddr] = do
  operand1 <- case a1 of
                Position x -> valueAtAddress x
                Immediate x -> return x
  operand2 <- case a2 of
                Position x -> valueAtAddress x
                Immediate x -> return x
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

instruction :: Int -> [ParameterMode] -> ([Operand] -> State Machine a) -> State Machine ()
instruction numParams modes effect = do
  (pc, memory, _) <- get
  let valuesInOperandPositions = map (\p -> memory ! (pc + p + 1)) [0..numParams-1]
  let operands = zipWith (\value mode -> case mode of
                                           PositionMode -> Position value
                                           ImmediateMode -> Immediate value)
                         valuesInOperandPositions
                         modes
  effect operands
  updateInstructionPointer (+ (numParams + 1))

addInstruction :: [ParameterMode] -> State Machine ()
addInstruction modes = instruction 3 modes $ executeBinaryOp (+)

multiplyInstruction :: [ParameterMode] -> State Machine ()
multiplyInstruction modes = instruction 3 modes $ executeBinaryOp (*)

haltInstruction :: [ParameterMode] -> State Machine ()
haltInstruction _ = instruction 0 [] $ const halt

decodeInstruction :: Int -> (Int, [ParameterMode])
decodeInstruction inst = (opcode, paramModes ++ repeat PositionMode)
  where
    (rest, opcode) = inst `divMod` 100
    paramModes = reverse . map (toEnum . read . pure) . show $ rest

decodeAndExecute :: Int -> State Machine ()
decodeAndExecute inst = action paramModes
  where
    (opcode, paramModes) = decodeInstruction inst
    action = case opcode of
               1 -> addInstruction
               2 -> multiplyInstruction
               99 -> haltInstruction

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, memory, _) <- get
  let opcode = memory ! pc
  decodeAndExecute opcode

runMachine :: State Machine ()
runMachine = do
  (_, _, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> State Machine a -> a
withMachine program action = evalState action (newMachine program)
