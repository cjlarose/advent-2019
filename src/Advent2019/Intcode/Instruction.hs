module Advent2019.Intcode.Instruction
  ( add
  , multiply
  , readInputOp
  , halt
  ) where

import Control.Monad (liftM2)
import Control.Monad.State (State, get, put, evalState)

import Advent2019.Intcode ( MachineState(..)
                          , Machine
                          , ParameterMode(..)
                          , Operand(..)
                          )
import Advent2019.Intcode.Machine ( valueAtAddress
                                  , writeToAddress
                                  , readInput
                                  , updateInstructionPointer
                                  )

resolveOperand :: Operand -> State Machine Int
resolveOperand (Position x) = valueAtAddress x
resolveOperand (Immediate x) = return x

instruction :: Int -> [ParameterMode] -> ([Operand] -> State Machine a) -> State Machine a
instruction numParams modes effect = do
  (pc, _, _, _) <- get
  valuesInOperandPositions <- mapM (\p -> valueAtAddress $ pc + p + 1) [0..numParams-1]
  let operands = zipWith (\mode -> case mode of
                                     PositionMode -> Position
                                     ImmediateMode -> Immediate)
                         modes
                         valuesInOperandPositions
  effect operands <* updateInstructionPointer (+ (numParams + 1))

binaryOp :: (Int -> Int -> Int) -> [ParameterMode] -> State Machine ()
binaryOp f modes = instruction 3 modes execute
  where
    execute [a1, a2, Position destAddr] =
      liftM2 f (resolveOperand a1) (resolveOperand a2) >>=
        writeToAddress destAddr

add :: [ParameterMode] -> State Machine ()
add = binaryOp (+)

multiply :: [ParameterMode] -> State Machine ()
multiply = binaryOp (*)

readInputOp :: [ParameterMode] -> State Machine ()
readInputOp modes = instruction 1 modes execute
  where
    execute [Position destAddr] = readInput >>= writeToAddress destAddr

halt :: [ParameterMode] -> State Machine ()
halt _ = instruction 0 [] . const $ do
  (ip, memory, input, status) <- get
  put (ip, memory, input ,Terminated)
