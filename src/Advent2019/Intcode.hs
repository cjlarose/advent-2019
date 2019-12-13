module Advent2019.Intcode
  ( MachineState(..)
  , Machine
  , IntcodeCompute
  , ParameterMode(..)
  , Operand(..)
  ) where

import Control.Monad.RWS (RWS)
import Data.Array.Unboxed (UArray)

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, [Int], MachineState)
type IntcodeCompute = RWS () [Int] Machine

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int
