module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , readInputOp
  , halt
  ) where

import Control.Monad.State (State, get, put, evalState)

import Advent2019.Intcode ( MachineState(..)
                          , Machine
                          , ParameterMode(..)
                          , Operand(..)
                          , valueAtAddress
                          , writeToAddress
                          , readInput
                          )

resolveOperand :: Operand -> State Machine Int
resolveOperand (Position x) = valueAtAddress x
resolveOperand (Immediate x) = return x

updateInstructionPointer :: (Int -> Int) -> State Machine ()
updateInstructionPointer f = do
  (pc, memory, input, status) <- get
  put (f pc, memory, input, status)

instruction :: Int -> [ParameterMode] -> ([Operand] -> State Machine a) -> State Machine ()
instruction numParams modes effect = do
  (pc, _, _, _) <- get
  valuesInOperandPositions <- mapM (\p -> valueAtAddress $ pc + p + 1) [0..numParams-1]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate)
                         modes
                         valuesInOperandPositions
  effect operands
  updateInstructionPointer (+ (numParams + 1))

binaryOp :: (Int -> Int -> Int) -> [ParameterMode] -> State Machine ()
binaryOp f modes = instruction 3 modes execute
  where
    execute [a1, a2, Position destAddr] = do
      operand1 <- resolveOperand a1
      operand2 <- resolveOperand a2
      let res = operand1 `f` operand2
      writeToAddress destAddr res

add :: [ParameterMode] -> State Machine ()
add = binaryOp (+)

multiply :: [ParameterMode] -> State Machine ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> State Machine ()
readInputOp modes = instruction 1 modes execute
  where
    execute [Position destAddr] = do
      val <- readInput
      writeToAddress destAddr val

halt :: [ParameterMode] -> State Machine ()
halt _ = instruction 0 [] . const $ do
  (ip, memory, input, status) <- get
  put (ip, memory, input ,Terminated)
