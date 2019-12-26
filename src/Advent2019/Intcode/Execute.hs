module Advent2019.Intcode.Execute
  ( withMachine
  , runMachine
  , evalMachine
  , runMachineWithInput
  , decodeInstruction
  ) where

import Control.Monad.RWS (evalRWS)
import Data.List (iterate')

import Advent2019.Intcode (Machine, TapeSymbol, IntcodeCompute, MachineState(..), ParameterMode(..))
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

decodeInstruction :: TapeSymbol -> (TapeSymbol, [ParameterMode])
decodeInstruction inst = (opcode, paramModes)
  where
    (rest, opcode) = inst `divMod` 100
    paramModes = map (toEnum . (`mod` 10)) . iterate' (`div` 10) . fromIntegral $ rest

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

withMachine :: [TapeSymbol] -> [TapeSymbol] -> IntcodeCompute a -> (a, [TapeSymbol])
withMachine program input action = evalMachine action . newMachine program $ input

runMachineWithInput :: [TapeSymbol] -> [TapeSymbol] -> [TapeSymbol]
runMachineWithInput program input = snd $ withMachine program input runMachine

evalMachine :: IntcodeCompute a -> Machine -> (a, [TapeSymbol])
evalMachine action = evalRWS action ()
