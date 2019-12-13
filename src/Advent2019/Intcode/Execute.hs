module Advent2019.Intcode.Execute
  ( withMachine
  , runMachine
  ) where

import Data.Array.IArray (listArray)
import Control.Monad.State (State, get, evalState)

import Advent2019.Intcode (MachineState(..), Machine, valueAtAddress, ParameterMode(..))
import Advent2019.Intcode.Instruction (addInstruction, multiplyInstruction, haltInstruction)

newMachine :: [Int] -> Machine
newMachine xs = (0, arr, Running)
  where
    n = length xs
    arr = listArray (0, n - 1) xs

decodeInstruction :: Int -> (Int, [ParameterMode])
decodeInstruction inst = (opcode, paramModes ++ repeat PositionMode)
  where
    (rest, opcode) = inst `divMod` 100
    paramModes = reverse . map (toEnum . read . pure) . show $ rest

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, _, _) <- get
  inst <- valueAtAddress pc
  let (opcode, paramModes) = decodeInstruction inst
  let action = case opcode of
                 1 -> addInstruction
                 2 -> multiplyInstruction
                 99 -> haltInstruction
  action paramModes

runMachine :: State Machine ()
runMachine = do
  (_, _, status) <- get
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Int] -> State Machine a -> a
withMachine program action = evalState action (newMachine program)
