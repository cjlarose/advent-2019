module Advent2019.Day14
  ( solve
  ) where

import Data.List (find, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Text.Parsec (many1, sepEndBy1, eof, sepBy1)
import Text.Parsec.Char (endOfLine, digit, space, char, upper)
import Text.Parsec.ByteString (Parser)
import Data.Tuple (swap)
import Numeric.Search.Integer (search)

import Advent2019.Input (getProblemInputAsByteString, withSuccessfulParse)

data Reaction = Reaction { reactants :: [(Int, String)]
                         , productR :: (Int, String)
                         } deriving Show
type ReactionMap = Map.Map String Reaction

reactions :: Parser [Reaction]
reactions = sepEndBy1 reaction endOfLine <* eof
  where
    quantityChemical = (\a b -> (a, b)) <$> (read <$> many1 digit <* many1 space) <*> many1 upper
    lhs = sepBy1 quantityChemical (char ',' >> many1 space)
    reaction = Reaction <$> (lhs <* many1 space <* char '=' <* char '>' <* many1 space) <*> quantityChemical

indexByProduct :: [Reaction] -> ReactionMap
indexByProduct = Map.fromList . map (\reaction -> (snd . productR $ reaction, reaction))

productionRule :: ReactionMap -> String -> Reaction
productionRule rs chem = rs ! chem

reactionChainDepth :: ReactionMap -> String -> Int
reactionChainDepth rs chem | chem == "ORE" = 0
                           | otherwise = (maximum . map (reactionChainDepth rs . snd) . reactants . productionRule rs $ chem) + 1

mergeDeps :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int
mergeDeps = Map.unionWith (+)

breakDownOneStep :: ReactionMap -> (Int, String) -> [(Int, String)]
breakDownOneStep rs (req, chem) = map (\(k, d) -> (k * multiplier, d)) deps
  where
    rule = productionRule rs chem
    deps = reactants rule
    yield = fst . productR $ rule
    multiplier = ceiling $ fromIntegral req / fromIntegral yield

requiredOre :: ReactionMap -> Map.Map String Int -> Int
requiredOre rs deps = if maxDepth == 0
                      then deps ! "ORE"
                      else requiredOre rs $ mergeDeps depsWithoutLongest newDeps
  where
    mostComplexDep = maximumBy (comparing $ reactionChainDepth rs) . Map.keys $ deps
    maxDepth = reactionChainDepth rs mostComplexDep
    depsWithoutLongest = Map.delete mostComplexDep deps
    newDeps = toDepMap . breakDownOneStep rs $ (deps ! mostComplexDep, mostComplexDep)

toDepMap :: [(Int, String)] -> Map.Map String Int
toDepMap = foldr (uncurry Map.insert . swap) Map.empty

fuelGivenOre :: ReactionMap -> Int -> Int
fuelGivenOre rs ore = (fromIntegral $ search (\k -> req (fromIntegral k) >= ore)) - 1
  where
    req k = requiredOre rs . toDepMap $ [(k, "FUEL")]

printResults :: [Reaction] -> (String, String)
printResults xs = (part1, part2)
  where
    reactionMap = indexByProduct xs
    part1 = show . requiredOre reactionMap . toDepMap $ [(1, "FUEL")]
    part2 = show $ fuelGivenOre reactionMap 1000000000000

solve :: IO (Either String (String, String))
solve = withSuccessfulParse reactions printResults <$> getProblemInputAsByteString 14
