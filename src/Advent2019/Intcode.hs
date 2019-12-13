module Advent2019.Intcode
  ( MachineState(..)
  , Machine
  , ParameterMode(..)
  , Operand(..)
  ) where

import Data.Array.Unboxed (UArray)

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, [Int], MachineState)

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int
