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

import Control.Monad (liftM2)
import Control.Monad.Writer (tell)

import Advent2019.Intcode ( IntcodeCompute
                          , ParameterMode(..)
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

instruction :: Int -> [ParameterMode] -> ([Operand] -> IntcodeCompute a) -> IntcodeCompute a
instruction numParams modes effect = do
  pc <- readInstructionPointer
  valuesInOperandPositions <- mapM valueAtAddress [pc + 1..pc + fromIntegral numParams]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate
                                     RelativeMode -> Relative)
                         modes
                         valuesInOperandPositions
  effect operands

nonJumpInstruction :: Int -> [ParameterMode] -> ([Operand] -> IntcodeCompute a) -> IntcodeCompute a
nonJumpInstruction numParams modes effect = instruction numParams modes effect <* updateInstructionPointer (+ (fromIntegral $ numParams + 1))

binaryOp :: (Integer -> Integer -> Integer) -> [ParameterMode] -> IntcodeCompute ()
binaryOp f modes = nonJumpInstruction 3 modes execute
  where
    execute [a1, a2, destAddr] = do
      result <- liftM2 f (resolveValueOperand a1) (resolveValueOperand a2)
      addr <- resolveAddressOperand destAddr
      writeToAddress addr result

add :: [ParameterMode] -> IntcodeCompute ()
add = binaryOp (+)

multiply :: [ParameterMode] -> IntcodeCompute ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> IntcodeCompute ()
readInputOp modes = nonJumpInstruction 1 modes execute
  where
    execute [destAddr] = do
      val <- readInput
      addr <- resolveAddressOperand destAddr
      writeToAddress addr val

writeOutput :: [ParameterMode] -> IntcodeCompute ()
writeOutput modes = nonJumpInstruction 1 modes execute
  where
    execute (operand:[]) = resolveValueOperand operand >>= tell . pure

jumpIfTrue :: [ParameterMode] -> IntcodeCompute ()
jumpIfTrue modes = instruction 2 modes execute
  where
    execute [a1, a2] = do
      operand1 <- resolveValueOperand a1
      operand2 <- resolveValueOperand a2
      if operand1 /= 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

jumpIfFalse :: [ParameterMode] -> IntcodeCompute ()
jumpIfFalse modes = instruction 2 modes execute
  where
    execute [a1, a2] = do
      operand1 <- resolveValueOperand a1
      operand2 <- resolveValueOperand a2
      if operand1 == 0
      then updateInstructionPointer (const operand2)
      else updateInstructionPointer (+ 3)

lessThan :: [ParameterMode] -> IntcodeCompute ()
lessThan = binaryOp (\a b -> fromIntegral . fromEnum $ a < b)

equals :: [ParameterMode] -> IntcodeCompute ()
equals = binaryOp (\a b -> fromIntegral . fromEnum $ a == b)

adjustRelativeBase :: [ParameterMode] -> IntcodeCompute ()
adjustRelativeBase modes = nonJumpInstruction 1 modes execute
  where
    execute [a1] = resolveValueOperand a1 >>= updateRelativeBase

halt :: [ParameterMode] -> IntcodeCompute ()
halt _ = instruction 0 [] . const $ setTerminated
