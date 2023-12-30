module Day21
  ( part1
  , part2
  ) where

import           Control.Monad.State (State, evalState, get, put)
import           Helpers.Parsers     (integers)

type Pos = Int

type Score = Int

type Player = (Pos, Score)

type Die = Int

type Rolls = Int

type Game1 = State (Player, Player, Die, Rolls) Int

type Game2 = State (Player, Player) (Int, Int)

dirac = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

advanceP1 :: Game1
advanceP1 = do
  ((p1pos, p1score), p2, die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newp1pos = mod (p1pos + roll - 1) 10 + 1
      p1 = (newp1pos, p1score + newp1pos)
      newdie = mod (die + 2) 100 + 1
      newRolls = rolls + 3
  put (p1, p2, newdie, newRolls)
  return $ score p2 newRolls

advanceP2 :: Game1
advanceP2 = do
  (p1, (p2pos, p2score), die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newp2pos = mod (p2pos + roll - 1) 10 + 1
      newdie = mod (die + 2) 100 + 1
      p2 = (newp2pos, p2score + newp2pos)
      newRolls = rolls + 3
  put (p1, p2, newdie, newRolls)
  return $ score p1 newRolls

score :: Player -> Rolls -> Int
score (_, score) rolls = score * rolls

game1 :: Game1
game1 = do
  s1 <- advanceP1
  ((_, p1score), _, _, _) <- get
  s2 <- advanceP2
  curState@(_, (_, p2score), _, _) <- get
  let result
        | p1score >= 1000 = s1
        | p2score >= 1000 = s2
        | otherwise = evalState game1 curState
  return result

toState :: String -> (Player, Player, Die, Rolls)
toState input = ((p1, 0), (p2, 0), 0, 0)
  where
    [[_, p1], [_, p2]] = integers input

part1 :: Bool -> String -> String
part1 _ = show . evalState game1 . toState

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
