module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , readInputOp
  , writeOutput
  , jumpIfTrue
  , jumpIfFalse
  , lessThan
  , equals
  , halt
  ) where

import Control.Monad (liftM2)
import Control.Monad.State (get, put, modify)
import Control.Monad.Writer (tell)

import Advent2019.Intcode ( MachineState(..)
                          , IntcodeCompute
                          , ParameterMode(..)
                          , Operand(..)
                          , Machine(..)
                          )
import Advent2019.Intcode.Machine ( valueAtAddress
                                  , writeToAddress
                                  , readInput
                                  , updateInstructionPointer
                                  )

resolveOperand :: Operand -> IntcodeCompute Integer
resolveOperand (Position x) = valueAtAddress x
resolveOperand (Immediate x) = return x

instruction :: Int -> [ParameterMode] -> ([Operand] -> IntcodeCompute a) -> IntcodeCompute a
instruction numParams modes effect = do
  pc <- instructionPointer <$> get
  valuesInOperandPositions <- mapM (\p -> valueAtAddress $ pc + (fromIntegral p) + 1) [0..numParams-1]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate)
                         modes
                         valuesInOperandPositions
  effect operands

nonJumpInstruction :: Int -> [ParameterMode] -> ([Operand] -> IntcodeCompute a) -> IntcodeCompute a
nonJumpInstruction numParams modes effect = instruction numParams modes effect <* updateInstructionPointer (+ (fromIntegral $ numParams + 1))

binaryOp :: (Integer -> Integer -> Integer) -> [ParameterMode] -> IntcodeCompute ()
binaryOp f modes = nonJumpInstruction 3 modes execute
  where
    execute [a1, a2, Position destAddr] =
      liftM2 f (resolveOperand a1) (resolveOperand a2) >>=
        writeToAddress destAddr

add :: [ParameterMode] -> IntcodeCompute ()
add = binaryOp (+)

multiply :: [ParameterMode] -> IntcodeCompute ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> IntcodeCompute ()
readInputOp modes = nonJumpInstruction 1 modes execute
  where
    execute [Position destAddr] = readInput >>= writeToAddress destAddr

writeOutput :: [ParameterMode] -> IntcodeCompute ()
writeOutput modes = nonJumpInstruction 1 modes execute
  where
    execute (operand:[]) = resolveOperand operand >>= tell . pure

jumpIfTrue :: [ParameterMode] -> IntcodeCompute ()
jumpIfTrue modes = instruction 2 modes execute
  where
    execute [a1, a2] = do
      operand1 <- resolveOperand a1
      operand2 <- resolveOperand a2
      if operand1 /= 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

jumpIfFalse :: [ParameterMode] -> IntcodeCompute ()
jumpIfFalse modes = instruction 2 modes execute
  where
    execute [a1, a2] = do
      operand1 <- resolveOperand a1
      operand2 <- resolveOperand a2
      if operand1 == 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

lessThan :: [ParameterMode] -> IntcodeCompute ()
lessThan = binaryOp (\a b -> fromIntegral . fromEnum $ a < b)

equals :: [ParameterMode] -> IntcodeCompute ()
equals = binaryOp (\a b -> fromIntegral . fromEnum $ a == b)

halt :: [ParameterMode] -> IntcodeCompute ()
halt _ = instruction 0 [] . const $
  modify $ (\m -> m { state = Terminated })
