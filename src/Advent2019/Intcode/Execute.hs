module Advent2019.Intcode.Execute
  ( withMachine
  , runMachine
  ) where

import Data.Array.IArray (listArray)
import Control.Monad.State (State, get, evalState)

import Advent2019.Intcode (MachineState(..), Machine, ParameterMode(..))
import Advent2019.Intcode.Instruction (add, multiply, readInputOp, halt)
import Advent2019.Intcode.Machine (valueAtAddress)

newMachine :: [Int] -> [Int] -> Machine
newMachine program input = (0, arr, input, Running)
  where
    n = length program
    arr = listArray (0, n - 1) program

decodeInstruction :: Int -> (Int, [ParameterMode])
decodeInstruction inst = (opcode, paramModes ++ repeat PositionMode)
  where
    (rest, opcode) = inst `divMod` 100
    paramModes = reverse . map (toEnum . read . pure) . show $ rest

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, _, _, _) <- get
  inst <- valueAtAddress pc
  let (opcode, paramModes) = decodeInstruction inst
  let action = case opcode of
                 1 -> add
                 2 -> multiply
                 3 -> readInputOp
                 99 -> halt
  action paramModes

runMachine :: State Machine ()
runMachine = do
  (_, _, _, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> [Int] -> State Machine a -> a
withMachine program input action = evalState action (newMachine program input)
