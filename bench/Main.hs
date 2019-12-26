module Main where

import qualified Text.Parsec (parse)
import Advent2019.Input (getProblemInputAsByteString)
import Advent2019.Intcode (TapeSymbol, ParameterMode, Machine)
import Advent2019.Intcode.Parse (program)
import Advent2019.Intcode.Machine (newMachine)
import Advent2019.Intcode.Execute (decodeInstruction, runMachine)
import Advent2019.Day19 (pulledByTractorBeam)

import Criterion.Main

decodeNoModes :: TapeSymbol -> TapeSymbol
decodeNoModes = fst . decodeInstruction

decode1Mode :: TapeSymbol -> (TapeSymbol, ParameterMode)
decode1Mode inst = (opcode, mode)
  where
    (opcode, [mode]) = decodeInstruction inst

decode2Modes :: TapeSymbol -> (TapeSymbol, ParameterMode, ParameterMode)
decode2Modes inst = (opcode, mode0, mode1)
  where
    (opcode, [mode0,mode1]) = decodeInstruction inst

decode3Modes :: TapeSymbol -> (TapeSymbol, ParameterMode, ParameterMode, ParameterMode)
decode3Modes inst = (opcode, mode0, mode1, mode2)
  where
    (opcode, [mode0,mode1,mode2]) = decodeInstruction inst

setupEnv :: IO ([TapeSymbol] -> Machine)
setupEnv = do
  problemInput <- getProblemInputAsByteString 19
  let parseResult = Text.Parsec.parse program "" problemInput
  case parseResult of
    Left err -> error . show $ err
    Right program -> pure $ newMachine program

main = defaultMain
  [ bgroup "decodeInstruction" [ bench "zero modes"  $ whnf decodeNoModes 01
                             , bench "one mode"    $ whnf decode1Mode 101
                             , bench "two modes"   $ whnf decode2Modes 2101
                             , bench "three modes" $ whnf decode3Modes 10201
                             ]
  , env setupEnv $ \machineFactory -> bgroup "pulledByTractorBeam"
    [ bench "origin" $ whnf (pulledByTractorBeam machineFactory) (0, 0)
    ]
  ]
