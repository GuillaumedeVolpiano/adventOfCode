-- This version of day 12 implements the "rake" approach, as described, amongst
--others, in https://www.reddit.com/r/adventofcode/comments/18hbjdi/2023_day_12_part_2_this_image_helped_a_few_people/
module Day12
  ( part1
  , part2
  ) where

import           Control.Parallel.Strategies (parMap, rpar)
import           Data.List                   (group, sort)
import           Data.Maybe                  (Maybe (Just, Nothing), catMaybes)
import           Helpers.Parsers             (custom, integers)

import           Debug.Trace

data State =
  State
    { curGroup     :: Int
    , amount       :: Int
    , permutations :: Int
    }
  deriving (Show)

instance Eq State where
  (State g1 a1 _) == (State g2 a2 _) = g1 == g2 && a1 == a2

instance Ord State where
  compare (State g1 a1 p1) (State g2 a2 p2)
    | g1 == g2 && a1 == a2 = compare p1 p2
    | g1 == g2 = compare a1 a2
    | otherwise = compare g1 g2

type Record = (Condition, Contiguous)

type Condition = String

type Contiguous = [Int]

rake :: [State] -> Record -> Int
rake states ([], cont) =
  sum .
  map permutations .
  filter
    (\(State g a _) ->
       g == length cont || g == (length cont - 1) && a == cont !! g) $
  states
rake states (s:ss, cont) = rake combined (ss, cont)
  where
    raked = concatMap (catMaybes . nextChar s cont) states
    combined =
      map (foldl1 (\(State g a p1) (State _ _ p2) -> State g a (p1 + p2))) .
      group . sort $
      raked

nextChar :: Char -> Contiguous -> State -> [Maybe State]
nextChar '?' cont state = nextChar '.' cont state ++ nextChar '#' cont state
nextChar '.' cont s@(State g a p)
  | g == length cont || a == 0 = [Just s]
  | a == cont !! g = [Just $ State (g + 1) 0 p]
  | otherwise = [Nothing]
nextChar '#' cont s@(State g a p)
  | g >= length cont || a == cont !! g = [Nothing]
  | otherwise = [Just $ State g (a + 1) p]

part1 :: Bool -> String -> String
part1 _ input = show . sum . parMap rpar (rake [State 0 0 1]) $ pairs
  where
    springs = map concat . custom "[?#.]+" $ input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ input = show . sum . parMap rpar (rake [State 0 0 1]) $ pairs
  where
    springs = map concat . custom "[?#.]+" $ input
    records = integers input
    unfoldedSprings =
      map (\t -> (t ++) . take (4 * (length t + 1)) . cycle $ ('?' : t)) springs
    unfoldedRecords = map (\t -> take (5 * length t) . cycle $ t) records
    pairs = zip unfoldedSprings unfoldedRecords
