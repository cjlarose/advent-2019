module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , readInputOp
  , writeOutput
  , jumpIfTrue
  , jumpIfFalse
  , lessThan
  , equals
  , adjustRelativeBase
  , halt
  ) where

import Control.Monad (zipWithM)
import Control.Monad.Writer (tell)

import Advent2019.Intcode ( IntcodeCompute
                          , ParameterMode(..)
                          , ParameterType(..)
                          )
import Advent2019.Intcode.Machine ( valueAtAddress
                                  , writeToAddress
                                  , readInput
                                  , readInstructionPointer
                                  , updateInstructionPointer
                                  , setTerminated
                                  , getRelativeBase
                                  , updateRelativeBase
                                  )

resolveOperand :: ParameterType -> ParameterMode -> Integer -> IntcodeCompute Integer
resolveOperand AddressParameter PositionMode val = pure val
resolveOperand AddressParameter RelativeMode val = (+ val) <$> getRelativeBase
resolveOperand ValueParameter ImmediateMode val = pure val
resolveOperand ValueParameter mode val = resolveOperand AddressParameter mode val >>= valueAtAddress

instruction :: [ParameterType] -> [ParameterMode] -> ([Integer] -> IntcodeCompute a) -> IntcodeCompute a
instruction paramTypes modes effect = do
  pc <- readInstructionPointer
  let operandResolvers = zipWith resolveOperand paramTypes modes
  let numParams = length paramTypes
  valuesInOperandPositions <- mapM valueAtAddress [pc + 1..pc + fromIntegral numParams]
  operands <- zipWithM ($) operandResolvers valuesInOperandPositions
  effect operands

nonJumpInstruction :: [ParameterType] -> [ParameterMode] -> ([Integer] -> IntcodeCompute a) -> IntcodeCompute a
nonJumpInstruction paramTypes modes effect = instruction paramTypes modes effect <* updateInstructionPointer (+ (fromIntegral $ numParams + 1))
  where numParams = length paramTypes

binaryOp :: (Integer -> Integer -> Integer) -> [ParameterMode] -> IntcodeCompute ()
binaryOp f modes = nonJumpInstruction [ValueParameter, ValueParameter, AddressParameter] modes execute
  where
    execute [a1, a2, destAddr] = writeToAddress destAddr $ f a1 a2

add :: [ParameterMode] -> IntcodeCompute ()
add = binaryOp (+)

multiply :: [ParameterMode] -> IntcodeCompute ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> IntcodeCompute ()
readInputOp modes = nonJumpInstruction [AddressParameter] modes execute
  where
    execute [destAddr] = readInput >>= writeToAddress destAddr

writeOutput :: [ParameterMode] -> IntcodeCompute ()
writeOutput modes = nonJumpInstruction [ValueParameter] modes execute
  where
    execute [operand] = tell . pure $ operand

jumpIfTrue :: [ParameterMode] -> IntcodeCompute ()
jumpIfTrue modes = instruction [ValueParameter, ValueParameter] modes execute
  where
    execute [operand1, operand2] =
      if operand1 /= 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

jumpIfFalse :: [ParameterMode] -> IntcodeCompute ()
jumpIfFalse modes = instruction [ValueParameter, ValueParameter] modes execute
  where
    execute [operand1, operand2] =
      if operand1 == 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

lessThan :: [ParameterMode] -> IntcodeCompute ()
lessThan = binaryOp (\a b -> fromIntegral . fromEnum $ a < b)

equals :: [ParameterMode] -> IntcodeCompute ()
equals = binaryOp (\a b -> fromIntegral . fromEnum $ a == b)

adjustRelativeBase :: [ParameterMode] -> IntcodeCompute ()
adjustRelativeBase modes = nonJumpInstruction [ValueParameter] modes execute
  where
    execute [a1] = updateRelativeBase a1

halt :: [ParameterMode] -> IntcodeCompute ()
halt _ = instruction [] [] . const $ setTerminated
