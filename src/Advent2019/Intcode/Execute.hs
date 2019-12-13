module Advent2019.Intcode.Execute
  ( withMachine
  , runMachine
  ) where

import Data.Array.IArray (listArray)
import Control.Monad.State (get)
import Control.Monad.RWS (evalRWS)

import Advent2019.Intcode (IntcodeCompute, MachineState(..), Machine, ParameterMode(..))
import Advent2019.Intcode.Instruction (add, multiply, readInputOp, writeOutput, halt)
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

executeOneInstruction :: IntcodeCompute ()
executeOneInstruction = do
  (pc, _, _, _) <- get
  inst <- valueAtAddress pc
  let (opcode, paramModes) = decodeInstruction inst
  let action = case opcode of
                 1 -> add
                 2 -> multiply
                 3 -> readInputOp
                 4 -> writeOutput
                 99 -> halt
  action paramModes

runMachine :: IntcodeCompute ()
runMachine = do
  (_, _, _, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> [Int] -> IntcodeCompute a -> (a, [Int])
withMachine program input action = evalRWS action () (newMachine program input)
