module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , halt
  ) where

import Control.Monad.State (State, get, put, evalState)

import Advent2019.Intcode ( MachineState(..)
                          , Machine
                          , ParameterMode(..)
                          , Operand(..)
                          , valueAtAddress
                          , writeToAddress
                          )

resolveOperand :: Operand -> State Machine Int
resolveOperand (Position x) = valueAtAddress x
resolveOperand (Immediate x) = return x

executeBinaryOp :: (Int -> Int -> Int) -> [Operand] -> State Machine ()
executeBinaryOp f [a1, a2, Position destAddr] = do
  operand1 <- resolveOperand a1
  operand2 <- resolveOperand a2
  let res = operand1 `f` operand2
  writeToAddress destAddr res

updateInstructionPointer :: (Int -> Int) -> State Machine ()
updateInstructionPointer f = do
  (pc, memory, status) <- get
  put (f pc, memory, status)

instruction :: Int -> [ParameterMode] -> ([Operand] -> State Machine a) -> State Machine ()
instruction numParams modes effect = do
  (pc, _, _) <- get
  valuesInOperandPositions <- mapM (\p -> valueAtAddress $ pc + p + 1) [0..numParams-1]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate)
                         modes
                         valuesInOperandPositions
  effect operands
  updateInstructionPointer (+ (numParams + 1))

add :: [ParameterMode] -> State Machine ()
add modes = instruction 3 modes $ executeBinaryOp (+)

multiply :: [ParameterMode] -> State Machine ()
multiply modes = instruction 3 modes $ executeBinaryOp (*)

halt :: [ParameterMode] -> State Machine ()
halt _ = instruction 0 [] . const $ do
  (ip, memory, status) <- get
  put (ip, memory, Terminated)
