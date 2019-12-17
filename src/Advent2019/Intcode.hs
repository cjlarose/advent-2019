module Advent2019.Intcode
  ( MachineState(..)
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , Operand(..)
  ) where

import Control.Monad.RWS (RWS)
import qualified Data.Map.Strict as Map

data MachineState = Running | Terminated
data Machine = Machine
  { instructionPointer :: Integer
  , memory :: Map.Map Integer Integer
  , input :: [Integer]
  , state :: MachineState }
type IntcodeCompute = RWS () [Integer] Machine

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Integer | Immediate Integer
