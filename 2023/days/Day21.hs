module Day21
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, array, bounds, indices, (!))
import           Data.Graph         (graphFromEdges)
import           Data.List          as L (filter, inits, map)
import           Data.List.Split    (chunksOf)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set           as St (Set, empty, filter, findMax, foldl,
                                           fromList, insert, map, notMember,
                                           null, size, toList, union, unions)
import           Helpers.Graph      (Pos, dirs, neighbours)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Garden = UArray Pos Char

startChar = 'S'

partTwoSteps = 26501365

setDir = fromList dirs

infiniteReach :: Garden -> Pos -> Set Pos
infiniteReach garden pos =
  fromList . L.filter ((/= '#') . (!) garden . vecMod) . L.map (pos +) $ dirs
  where
    (_, V2 mx _) = bounds garden
    w = mx + 1
    vecMod (V2 x y) = V2 (x `mod` w) (y `mod` w)

reachable :: Garden -> Pos -> Set Pos
reachable garden =
  fromList . L.filter ((/= '#') . (!) garden) . neighbours garden

reachBool :: Garden -> Pos -> Bool
reachBool garden = not . St.null . reachable garden

doStep :: Garden -> (Set Pos, Set Pos) -> (Set Pos, Set Pos)
doStep garden (cur, seen) =
  ( unions . St.map (St.filter (`notMember` newSeen) . reachable garden) $ cur
  , newSeen)
  where
    newSeen = cur `union` seen

infiniteStep :: Garden -> (Set Pos, Set Pos) -> (Set Pos, Set Pos)
infiniteStep garden (cur, seen) =
  ( unions . St.map (St.filter (`notMember` newSeen) . infiniteReach garden) $
    cur
  , newSeen)
  where
    newSeen = cur `union` seen

setReach :: Garden -> Pos -> Int -> Set Pos
setReach garden startPos steps =
  St.filter ((== steps `mod` 2) . vPar) .
  unions . L.map (fst . (!!) allSteps) . takeWhile (<= steps) $
  [0 ..]
  where
    allSteps =
      take (steps + 1) . iterate (doStep garden) $ (fromList [startPos], empty)
    vPar (V2 x y) = (x + y) `mod` 2

setInfiniteReach :: Garden -> Pos -> Int -> Set Pos
setInfiniteReach garden startPos steps =
  St.filter ((== steps `mod` 2) . vPar) .
  unions . L.map (fst . (!!) allSteps) . takeWhile (<= steps) $
  [0 ..]
  where
    allSteps =
      take (steps + 1) . iterate (infiniteStep garden) $
      (fromList [startPos], empty)
    vPar (V2 x y) = (x + y) `mod` 2

part1 :: Bool -> String -> String
part1 test input = show . size . setReach garden startPos $ steps
  where
    garden = arrayFromString input
    startPos = head . L.filter ((== 'S') . (!) garden) . indices $ garden
    steps
      | test = 6
      | otherwise = 64

part2 :: Bool -> String -> String
part2 _ input = show result
  where
    garden = arrayFromString input
    (_, V2 mx my) = bounds garden
    startPos@(V2 sx sy) =
      head . L.filter ((== 'S') . (!) garden) . indices $ garden
    -- If there were no rocks, the number of plots that could be visited in n
    -- turns would be the area of a square with diagonal (2n + 1). If a is the side,
    -- then we have a² + a² = n² (by Pythagoras). As the area of the square is
    -- a², it is also (2n + 1)²/2, rounded up if n is even. The number of
    -- rocks is also going to expand quadratically if we move by steps of w = (mx +
    -- 1).
    -- Anyhow, the area visitable will follow a quadratic with equation a(x*w)²
    -- + b(x*w)+c.
    -- We can find a b and c by considering the points x ==0, x ==1 and x==2,
    -- c is the offset, the size of the visitable space when x == 0, that is n =
    -- mod partTwoSteps w. Also, we only consider odd steps or even steps,
    -- depending on the parity of partTwoSteps
    w = mx + 1
    remainder = mod partTwoSteps w
    divisor = div partTwoSteps w
    zero = size . setInfiniteReach garden startPos $ remainder
    one = size . setInfiniteReach garden startPos $ w + remainder
    two = size . setInfiniteReach garden startPos $ 2 * w + remainder
      -- we have a + b + c = one
      -- 4a + 2b + c = two
      -- So 2a = two + c - 2one = two - 3*zero - 2*one
      -- and b = one - zero - a
    a = div (two + c - 2 * one) 2
    b = one - c - a
    c = zero
    three
      | odd partTwoSteps =
        size . setInfiniteReach garden startPos $ 3 * w + remainder
      | even partTwoSteps =
        size . setInfiniteReach garden startPos $ 3 * w + remainder
    predictThree = 9 * a + 3 * b + c
    -- from there, we get the result, hopefully
    result = a * (divisor ^ 2) + b * divisor + zero
