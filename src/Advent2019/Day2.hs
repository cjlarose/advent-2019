module Advent2019.Day2
  ( solve
  , runProgram
  ) where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Array.IArray (IArray, listArray, (!), (//))
import Data.Array.Unboxed (UArray)
import Control.Monad.State (State, get, put, evalState)

import Text.Parsec (many1, sepBy1, eof)
import Text.Parsec.Char (endOfLine, digit, char)
import Text.Parsec.ByteString (Parser)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

program :: Parser [Int]
program = sepBy1 instruction (char ',') <* endOfLine <* eof
  where
    instruction = read <$> many1 digit

data MachineState = Running | Terminated
type Machine = (Int, UArray Int Int, MachineState)

newMachine :: [Int] -> Machine
newMachine xs = (0, arr, Running)
  where
    n = length xs
    arr = listArray (0, n - 1) xs

dereference :: UArray Int Int -> Int -> Int
dereference arr i = arr ! address
  where address = arr ! i

executeBinaryOp :: (Int -> Int -> Int) -> UArray Int Int -> Int -> UArray Int Int
executeBinaryOp f memory pc = memory // [(dest, res)]
  where
    operand1 = dereference memory $ pc + 1
    operand2 = dereference memory $ pc + 2
    dest = memory ! (pc + 3)
    res = operand1 `f` operand2

executeOneInstruction :: State Machine ()
executeOneInstruction = do
  (pc, memory, _) <- get
  let opcode = memory ! pc
  case opcode of
    1 -> do
      let newMemory = executeBinaryOp (+) memory pc
      put (pc + 4, newMemory, Running)
    2 -> do
      let newMemory = executeBinaryOp (*) memory pc
      put (pc + 4, newMemory, Running)
    99 -> put (pc, memory, Terminated)

runMachine :: State Machine Int
runMachine = do
  (_, memory, status) <- get
  case status of
    Terminated -> return $ memory ! 0
    Running -> executeOneInstruction >> runMachine

updateMemory :: [(Int, Int)] -> State Machine ()
updateMemory updates = do
  (pc, memory, status) <- get
  let newMemory = memory // updates
  put (pc, newMemory, status)

runProgram :: [Int] -> [(Int, Int)] -> Int
runProgram xs updates = evalState (updateMemory updates >> runMachine) (newMachine xs)

runProgramWithInputs :: [Int] -> (Int, Int) -> Int
runProgramWithInputs xs (a, b) = runProgram xs [(1, a), (2, b)]

findNounVerb :: [Int] -> Int -> (Int, Int)
findNounVerb xs n = fst . fromJust . find ((== n) . snd) . map (\p -> (p, runProgramWithInputs xs p)) $ [(a, b) | a <- [0..99], b <- [0..99]]

printResults :: [Int] -> IO ()
printResults xs = do
  let part1 = runProgramWithInputs xs (12, 2)
  let (noun, verb) = findNounVerb xs 19690720
  putStrLn . show $ part1
  putStrLn . show $ 100 * noun + verb

solve :: IO ()
solve = getProblemInputAsByteString 2 >>= withSuccessfulParse program printResults