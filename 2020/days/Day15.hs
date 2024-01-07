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

doNRounds :: Int -> State -> Int 
doNRounds nturns (turn, val, game) 
    | nturns == turn = val
    | otherwise = doNRounds nturns (turn + 1, newVal, insert val turn game)
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

part1 :: Bool -> String -> String
part1 _ =
  show .
  doNRounds 2020 . createGame .
  zipWith (\a b -> (b, a)) [1 ..] .
  map Prelude.read . splitOn "," . filter (/= '\n')

part2 :: Bool -> String -> String
part2 _ =
  show .
  doNRounds 30000000 . createGame .
  zipWith (\a b -> (b, a)) [1 ..] .
  map Prelude.read . splitOn "," . filter (/= '\n')
