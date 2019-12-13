module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , readInputOp
  , halt
  ) where

import Control.Monad (liftM2)
import Control.Monad.State (State, get, put, evalState)

import Advent2019.Intcode ( MachineState(..)
                          , IntcodeCompute
                          , ParameterMode(..)
                          , Operand(..)
                          )
import Advent2019.Intcode.Machine ( valueAtAddress
                                  , writeToAddress
                                  , readInput
                                  , updateInstructionPointer
                                  )

resolveOperand :: Operand -> IntcodeCompute Int
resolveOperand (Position x) = valueAtAddress x
resolveOperand (Immediate x) = return x

instruction :: Int -> [ParameterMode] -> ([Operand] -> IntcodeCompute a) -> IntcodeCompute a
instruction numParams modes effect = do
  (pc, _, _, _) <- get
  valuesInOperandPositions <- mapM (\p -> valueAtAddress $ pc + p + 1) [0..numParams-1]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate)
                         modes
                         valuesInOperandPositions
  effect operands <* updateInstructionPointer (+ (numParams + 1))

binaryOp :: (Int -> Int -> Int) -> [ParameterMode] -> IntcodeCompute ()
binaryOp f modes = instruction 3 modes execute
  where
    execute [a1, a2, Position destAddr] =
      liftM2 f (resolveOperand a1) (resolveOperand a2) >>=
        writeToAddress destAddr

add :: [ParameterMode] -> IntcodeCompute ()
add = binaryOp (+)

multiply :: [ParameterMode] -> IntcodeCompute ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> IntcodeCompute ()
readInputOp modes = instruction 1 modes execute
  where
    execute [Position destAddr] = readInput >>= writeToAddress destAddr

halt :: [ParameterMode] -> IntcodeCompute ()
halt _ = instruction 0 [] . const $ do
  (ip, memory, input, status) <- get
  put (ip, memory, input ,Terminated)
