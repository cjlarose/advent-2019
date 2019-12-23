module Advent2019.Intcode
  ( MachineState(..)
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , ParameterType(..)
  , Operand(..)
  ) where

import Control.Monad.RWS (RWS)
import qualified Data.Map.Strict as Map

data MachineState = Running | Terminated
data Machine = Machine
  { instructionPointer :: Integer
  , relativeBase :: Integer
  , memory :: Map.Map Integer Integer
  , input :: [Integer]
  , state :: MachineState }
type IntcodeCompute = RWS () [Integer] Machine

data ParameterMode = PositionMode | ImmediateMode | RelativeMode deriving (Enum)
data ParameterType = ValueParameter | AddressParameter
data Operand = Position Integer | Immediate Integer | Relative Integer
