module Day21
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, indices, (!))
import           Data.List          as L (filter, map)
import           Data.Set           as St (Set, empty, filter, foldl, fromList,
                                           map, notMember, size, toList, union,
                                           unions)
import           Helpers.Graph      (Pos, dirs, neighbours)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

startChar = 'S'

partTwoSteps = 26501365

setDir = fromList dirs

reachable :: UArray Pos Char -> Pos -> Set Pos
reachable garden =
  fromList . L.filter ((/= '#') . (!) garden) . neighbours garden

infiniteNeighbours :: UArray Pos Char -> Pos -> Set Pos
infiniteNeighbours garden pos =
  St.filter ((/= '#') . (!) garden) . St.map (modVec . (pos +)) $ setDir
  where
    (_, V2 mx my) = bounds garden
    modVec (V2 x y) = V2 (mod x (mx + 1)) (mod y (my + 1))

doStep :: UArray Pos Char -> Set Pos -> Set Pos
doStep garden = unions . St.map (reachable garden)

otherStep :: UArray Pos Char -> (Set Pos, Set Pos) -> (Set Pos, Set Pos)
otherStep garden (cur, seen) =
  ( unions . St.map (St.filter (`notMember` newSeen) . reachable garden) $ cur
  , newSeen)
  where
    newSeen = cur `union` seen

powersOfTwo :: Int -> [Int]
powersOfTwo i = 0 : [2 ^ x | x <- [1 .. round . logBase 2 . fromIntegral $ i]]

part1 :: Bool -> String -> String
part1 test input = show . size $ result
  where
    garden = arrayFromString input
    startPos = head . L.filter ((== 'S') . (!) garden) . indices $ garden
    steps
      | test = 6
      | otherwise = 64
    allSteps =
      take (steps + 1) . iterate (otherStep garden) $
      (fromList [startPos], empty)
    -- result = unions . L.map fst $ allSteps
    result =
      unions $
      fst (allSteps !! steps) : L.map (fst . (!!) allSteps) [0,2 .. steps - 1]

part2 :: Bool -> String -> String
part2 _ input = "Part 2"
