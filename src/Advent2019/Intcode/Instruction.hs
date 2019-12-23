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
                          , Operand(..)
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

resolveValueOperand :: Operand -> IntcodeCompute Integer
resolveValueOperand (Immediate x) = return x
resolveValueOperand addr = resolveAddressOperand addr >>= valueAtAddress

resolveAddressOperand :: Operand -> IntcodeCompute Integer
resolveAddressOperand (Position absoluteAddr) = pure absoluteAddr
resolveAddressOperand (Relative relativeAddr) = (+ relativeAddr) <$> getRelativeBase

resolveOperand :: ParameterType -> ParameterMode -> Integer -> IntcodeCompute Integer
resolveOperand paramType mode val = f paramType $ op mode
  where
    op PositionMode = Position val
    op ImmediateMode = Immediate val
    op RelativeMode = Relative val
    f AddressParameter = resolveAddressOperand
    f ValueParameter = resolveValueOperand

instruction :: [ParameterType] -> [ParameterMode] -> ([Integer] -> IntcodeCompute a) -> IntcodeCompute a
instruction paramTypes modes effect = do
  pc <- readInstructionPointer
  let operandResolvers = zipWith resolveOperand paramTypes modes
  let numParams = length paramTypes
  valuesInOperandPositions <- mapM valueAtAddress [pc + 1..pc + fromIntegral numParams]
  operands <- zipWithM (\f x -> f x) operandResolvers valuesInOperandPositions
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
