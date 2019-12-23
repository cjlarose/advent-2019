module Advent2019.Intcode.Execute
  ( withMachine
  , runMachine
  , runMachineWithInput
  ) where

import Control.Monad.RWS (evalRWS)

import Advent2019.Intcode (IntcodeCompute, MachineState(..), ParameterMode(..))
import Advent2019.Intcode.Instruction ( add
                                      , multiply
                                      , readInputOp
                                      , writeOutput
                                      , jumpIfTrue
                                      , jumpIfFalse
                                      , lessThan
                                      , equals
                                      , adjustRelativeBase
                                      , halt)
import Advent2019.Intcode.Machine (newMachine, valueAtAddress, readInstructionPointer, getStatus)

decodeInstruction :: Integer -> (Integer, [ParameterMode])
decodeInstruction inst = (opcode, paramModes (fromIntegral rest) ++ repeat PositionMode)
  where
    (rest, opcode) = inst `divMod` 100

    paramModes 0 = []
    paramModes n = let (dividend, rem) = n `divMod` 10 in toEnum rem : paramModes dividend

executeOneInstruction :: IntcodeCompute ()
executeOneInstruction = do
  pc <- readInstructionPointer
  inst <- valueAtAddress pc
  let (opcode, paramModes) = decodeInstruction inst
  let action = case opcode of
                 1 -> add
                 2 -> multiply
                 3 -> readInputOp
                 4 -> writeOutput
                 5 -> jumpIfTrue
                 6 -> jumpIfFalse
                 7 -> lessThan
                 8 -> equals
                 9 -> adjustRelativeBase
                 99 -> halt
  action paramModes

runMachine :: IntcodeCompute ()
runMachine = do
  status <- getStatus
  case status of
    Terminated -> return ()
    Running -> executeOneInstruction >> runMachine

withMachine :: [Integer] -> [Integer] -> IntcodeCompute a -> (a, [Integer])
withMachine program input action = evalRWS action () (newMachine program input)

runMachineWithInput :: [Integer] -> [Integer] -> [Integer]
runMachineWithInput program input = snd $ withMachine program input runMachine
