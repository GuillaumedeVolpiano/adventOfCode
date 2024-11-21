module Day3
  ( part1
  , part2
  ) where

import           Data.List  (unfoldr)
import           Data.Map   as M (Map, empty, insert, lookup)
import           Data.Maybe (catMaybes)

findCoords :: Int -> (Int, Int)
findCoords k = (x, y)
  where
    closestSqrt = ceiling . sqrt . fromIntegral $ k
    closestOddSqrt
      | even closestSqrt = closestSqrt + 1
      | otherwise = closestSqrt
    startPoint = closestOddSqrt ^ 2
    baseCoord = div closestOddSqrt 2
    (x, y) = walkBack startPoint (baseCoord, (-1) * baseCoord)
    walkBack origin (a, b)
      | origin - k < closestOddSqrt = (a + k - origin, b)
      | otherwise =
        walkUp (origin - closestOddSqrt + 1) (a - closestOddSqrt + 1, b)
    walkUp origin (a, b)
      | origin - k < closestOddSqrt = (a, b + origin - k)
      | otherwise =
        walkFront (origin - closestOddSqrt + 1) (a, b + closestOddSqrt - 1)
    walkFront origin (a, b)
      | origin - k < closestOddSqrt = (a + origin - k, b)
      | otherwise =
        walkDown (origin - closestOddSqrt + 1) (a + closestOddSqrt - 1, b)
    walkDown origin (a, b) = (a, b + k - origin)

distance :: (Int, Int) -> Int
distance (x, y) = abs x + abs y

spiral :: (Int, Map (Int, Int) Int) -> Maybe (Int, (Int, Map (Int, Int) Int))
spiral (counter, memory)
  | counter == 1 = Just (1, (2, insert (0, 0) 1 memory))
  | otherwise = Just (newVal, (counter + 1, insert (x, y) newVal memory))
  where
    (x, y) = findCoords counter
    newVal =
      sum . catMaybes
        $ [ M.lookup (x + a, y + b) memory
          | a <- [(-1) .. 1]
          , b <- [(-1) .. 1]
          , (a, b) /= (0, 0)
          ]

part1 :: Bool -> String -> String
part1 _ = show . distance . findCoords . read

part2 :: Bool -> String -> String
part2 _ input = show . head . filter (> goal) . unfoldr spiral $ (1, empty)
  where
    goal = read input :: Int
