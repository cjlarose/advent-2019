module Advent2019.Intcode
  ( MachineState(..)
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , Operand(..)
  ) where

import Control.Monad.RWS (RWS)
import Data.Array.Unboxed (UArray)

data MachineState = Running | Terminated
data Machine = Machine
  { instructionPointer :: Int
  , memory :: UArray Int Int
  , input :: [Int]
  , state :: MachineState }
type IntcodeCompute = RWS () [Int] Machine

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int
