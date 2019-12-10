module Advent2019.Input
  ( getProblemInputAsString
  , getProblemInputAsByteString
  , withSuccessfulParse
  ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.ByteString as B
import Text.Parsec (ParseError)
import qualified Text.Parsec (parse)
import Text.Parsec.ByteString (Parser)

path :: Int -> String
path problemNumber = "inputs/" ++ (show problemNumber) ++ ".txt"

getProblemInputAsString :: Int -> IO String
getProblemInputAsString = readFile . path

getProblemInputAsByteString :: Int -> IO B.ByteString
getProblemInputAsByteString = B.readFile . path

handleParseError :: ParseError -> IO ()
handleParseError err = do
  hPutStrLn stderr "Parse error:"
  hPutStrLn stderr . show $ err
  exitFailure

withSuccessfulParse :: Parser a -> (a -> IO ()) -> B.ByteString -> IO ()
withSuccessfulParse p f input = either handleParseError f . Text.Parsec.parse p "" $ input
