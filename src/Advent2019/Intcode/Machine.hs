module Advent2019.Intcode.Machine
  ( writeToAddress
  , valueAtAddress
  , readInput
  ) where

import Control.Monad.State (State, get, put)
import Data.Array.Unboxed ((//), (!))

import Advent2019.Intcode (Machine)

updateMemory :: [(Int, Int)] -> State Machine ()
updateMemory updates = do
  (pc, memory, input, status) <- get
  let newMemory = memory // updates
  put (pc, newMemory, input, status)

writeToAddress :: Int -> Int -> State Machine ()
writeToAddress addr val = updateMemory [(addr, val)]

valueAtAddress :: Int -> State Machine Int
valueAtAddress addr = do
  (_, memory, _, _) <- get
  pure $ memory ! addr

readInput :: State Machine Int
readInput = do
  (pc, memory, input, status) <- get
  let val = head input
  put (pc, memory, tail input, status)
  pure val
