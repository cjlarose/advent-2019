module Advent2019.Intcode
  ( MachineState(..)
  , Machine
  , ParameterMode(..)
  , Operand(..)
  , valueAtAddress
  , writeToAddress
  ) where

import Control.Monad.State (State, get, put)
import Data.Array.Unboxed (UArray, (//), (!))

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, MachineState)

data ParameterMode = PositionMode | ImmediateMode deriving (Enum)
data Operand = Position Int | Immediate Int

updateMemory :: [(Int, Int)] -> State Machine ()
updateMemory updates = do
  (pc, memory, status) <- get
  let newMemory = memory // updates
  put (pc, newMemory, status)

writeToAddress :: Int -> Int -> State Machine ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Int -> State Machine Int
valueAtAddress addr = do
  (_, memory, _) <- get
  pure $ memory ! addr

