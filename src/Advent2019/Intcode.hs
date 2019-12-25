module Advent2019.Intcode
  ( MachineState(..)
  , TapeSymbol
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , ParameterType(..)
  ) where

import Data.Int (Int64)
import Control.Monad.RWS (RWS)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

data MachineState = Running | Terminated
type TapeSymbol = Int64
data Machine = Machine
  { instructionPointer :: TapeSymbol
  , relativeBase :: TapeSymbol
  , memory :: HashMap TapeSymbol TapeSymbol
  , input :: [TapeSymbol]
  , state :: MachineState }
type IntcodeCompute = RWS () [TapeSymbol] Machine

data ParameterMode = PositionMode | ImmediateMode | RelativeMode deriving (Enum)
data ParameterType = ValueParameter | AddressParameter
