module Day15
  ( part1
  , part2
  ) where

import           Data.IntMap     as M (IntMap, fromList, insert, lookup)
import           Data.List.Split (splitOn)
import           Data.Maybe      (Maybe (Just, Nothing), fromJust, isNothing)

type Game = IntMap Int

type Turn = Int

type Val = Int

type State = (Turn, Val, Game)

doRound :: State -> State 
doRound (turn, val, game) = (turn + 1, newVal, insert val turn game)
  where
    newVal
      | isNothing lastSaid = 0
      | otherwise = turn - fromJust lastSaid
    lastSaid = M.lookup val game

createGame :: [(Int, Int)] -> State
createGame prelude = (turn, val, game)
  where
    game = fromList . init $ prelude
    (val, turn) = last prelude

afterN :: Int -> [(Int, Int)] -> Int
afterN nturns list = (\(_, r, _) -> r) . last . take (nturns - length list + 1) . iterate doRound . createGame $ list

part1 :: Bool -> String -> String
part1 _ =
  show .
  afterN 2020 .
  zipWith (\a b -> (b, a)) [1 ..] .
  map Prelude.read . splitOn "," . filter (/= '\n')

part2 :: Bool -> String -> String
part2 _ =
  show .
  afterN 30000000 .
  zipWith (\a b -> (b, a)) [1 ..] .
  map Prelude.read . splitOn "," . filter (/= '\n')
