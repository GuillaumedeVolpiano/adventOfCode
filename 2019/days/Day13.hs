module Day13
  ( part1
  , part2
  ) where

import           Control.Lens.Getter ((^.))
import           Data.Bifunctor      (first, second)
import           Data.HashMap.Lazy   as M (HashMap, filter, findWithDefault,
                                           fromList, insert, keys, null, size,
                                           (!))
import           Data.List.Split     (chunksOf)
import           Intcode             (Intcode, clearOutput, initialise,
                                      runIntcode, sendInput, setMemory)
import           Linear.V2           (V2 (..), _x, _y)

type Screen = HashMap Tile Int

type Tile = V2 Int

type Game = HashMap Tile Int

firstTurn :: Intcode -> (Game, Intcode)
firstTurn = first gamify . runIntcode

play :: (Game, Intcode) -> Int
play (game, machine)
  | M.null blocks = game ! V2 (-1) 0
  | otherwise = play (newGame, newMachine)
  where
    ball = (^. _x) . head . keys . M.filter (== 4) $ game
    blocks = M.filter (== 2) game
    paddle = (^. _x) . head . keys . M.filter (== 3) $ game
    move = signum (ball - paddle)
    (updateOutput, newMachine) =
      runIntcode . sendInput move . clearOutput $ machine
    newGame = foldr (\(a, b) c -> insert a b c) game . tilify $ updateOutput

tilify :: [Int] -> [(Tile, Int)]
tilify = map (\[a, b, c] -> (V2 c b, a)) . chunksOf 3

gamify :: [Int] -> Game
gamify = fromList . tilify

part1 :: Bool -> String -> String
part1 _ = show . size . M.filter (== 2) . fst . firstTurn . initialise

part2 :: Bool -> String -> String
part2 _ = show . play . firstTurn . setMemory 0 2 . initialise
