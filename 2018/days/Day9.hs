module Day9
  ( part1
  , part2
  ) where

import           Data.IntMap     as M (IntMap, alter, elems, empty)
import           Data.Maybe      (Maybe (Just, Nothing))
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), empty, length,
                                        splitAt, (><))
import           Helpers.Parsers (integers)

turn :: Int -> (Int, Seq Int, IntMap Int) -> (Int, Seq Int, IntMap Int)
turn players (turn, state, scores) = (turn + 1, newState, newScores)
  where
    curPlayer = mod turn players
    newState
      | ls <= 1 = turn :<| state
      | mod turn 23 == 0 && turn /= 0 = rest >< prev
      | otherwise = turn :<| (ate :|> s :|> t)
    (s :<| t :<| ate) = state
    newScores
      | mod turn 23 == 0 && turn /= 0 =
        alter (updateScore (turn + marble)) curPlayer scores
      | otherwise = scores
    updateScore v Nothing  = Just v
    updateScore v (Just s) = Just (s + v)
    ls = Sq.length state
    (prev, marble :<| rest) = Sq.splitAt (ls - 7) state

play :: [[Int]] -> Int
play [[players, lastMarble]] =
  maximum .
  (\(_, _, scores) -> elems scores) . (!! lastMarble) . iterate (turn players) $
  (0, Sq.empty, M.empty)

part1 :: Bool -> String -> String
part1 _ = show . play . integers

part2 :: Bool -> String -> String
part2 _ = show . play . (\[[p, l]] -> [[p, l * 100]]) . integers
