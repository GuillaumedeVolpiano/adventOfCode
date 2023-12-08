module Day2
  ( part1
  , part2
  ) where

import           Linear.V2       (V2 (..))
import           Text.Regex.TDFA (getAllTextMatches, (=~))

type Command = (String, Int)

type Pos = V2 Int

parseLine :: String -> [Command]
parseLine =
  map
    ((\(a, _, b) -> (a, read b)) . (\l -> l =~ " " :: (String, String, String))) .
  lines

plainCommands :: Pos -> [Command] -> Pos
plainCommands p [] = p
plainCommands p ((d, v):xs) = plainCommands newP xs
  where
    newP
      | d == "forward" = p + V2 v 0
      | d == "up" = p + V2 0 (-v)
      | d == "down" = p + V2 0 v

score :: Pos -> Int
score (V2 x y) = x * y

aimCommands :: Pos -> Int -> [Command] -> Pos
aimCommands p _ [] = p
aimCommands p aim ((d, v):xs) = aimCommands newP newA xs
  where
    newP
      | d == "forward" = p + V2 v (aim * v)
      | otherwise = p
    newA
      | d == "up" = aim - v
      | d == "down" = aim + v
      | otherwise = aim

part1 :: Bool -> String -> String
part1 _ = show . score . plainCommands (V2 0 0) . parseLine

part2 :: Bool -> String -> String
part2 _ = show . score . aimCommands (V2 0 0) 0 . parseLine
