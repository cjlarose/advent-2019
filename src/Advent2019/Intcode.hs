module Advent2019.Intcode
  ( MachineState(..)
  , TapeSymbol
  , Machine(..)
  , IntcodeCompute
  , ParameterMode(..)
  , ParameterType(..)
  ) where

import Data.Int (Int64)
import Control.Monad.State (State)
import Control.Monad.Writer (WriterT)
import Data.Vector.Unboxed (Vector)

data MachineState = Running | Terminated
type TapeSymbol = Int64
data Machine = Machine
  { instructionPointer :: TapeSymbol
  , relativeBase :: TapeSymbol
  , memory :: Vector TapeSymbol
  , input :: [TapeSymbol]
  , state :: MachineState }
type IntcodeCompute = WriterT [TapeSymbol] (State Machine)

data ParameterMode = PositionMode | ImmediateMode | RelativeMode deriving (Enum)
data ParameterType = ValueParameter | AddressParameter
