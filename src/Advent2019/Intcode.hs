module Advent2019.Intcode
  ( MachineState(..)
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , ParameterType(..)
  ) where

import Control.Monad.RWS (RWS)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

data MachineState = Running | Terminated
data Machine = Machine
  { instructionPointer :: Integer
  , relativeBase :: Integer
  , memory :: HashMap Integer Integer
  , input :: [Integer]
  , state :: MachineState }
type IntcodeCompute = RWS () [Integer] Machine

data ParameterMode = PositionMode | ImmediateMode | RelativeMode deriving (Enum)
data ParameterType = ValueParameter | AddressParameter
