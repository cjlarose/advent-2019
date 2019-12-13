module Advent2019.Intcode
  ( MachineState(..)
  , Machine
  , IntcodeCompute
  , ParameterMode(..)
  , Operand(..)
  ) where

import Control.Monad.State (State)
import Data.Array.Unboxed (UArray)

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, [Int], MachineState)
type IntcodeCompute = State Machine

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int
