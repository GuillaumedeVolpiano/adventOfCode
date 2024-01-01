module Day21
  ( part1
  , part2
  ) where

import           Control.Monad.State (State, evalState, get, put)
import           Data.MultiSet       as MS (MultiSet, Occur, empty, foldOccur,
                                            fromList, insertMany, partition,
                                            singleton, size, union)
import           Helpers.Parsers     (integers)
import           Linear.V2           (V2 (..))

type Pos = Int

type Score = Int

data Player =
  Player Pos Score
  deriving (Show, Eq, Ord)

type Die = Int

type Rolls = Int

type Win = V2 Int

type Dice = MultiSet Int

type Games = MultiSet (Player, Player)

type Game1 = State (Player, Player, Die, Rolls) Int

type Game2 = State Games Win

dirac = fromList [x + y + z | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]

advance :: Player -> Die -> Player
advance (Player pos score) roll = Player newPos (score + newPos)
  where
    newPos = mod (pos + roll - 1) 10 + 1

advanceP1 :: Game1
advanceP1 = do
  (p1, p2, die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newP1 = advance p1 roll
      newdie = mod (die + 2) 100 + 1
      newRolls = rolls + 3
  put (newP1, p2, newdie, newRolls)
  return $ score p2 newRolls

advanceP2 :: Game1
advanceP2 = do
  (p1, p2, die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newP2 = advance p2 roll
      newdie = mod (die + 2) 100 + 1
      newRolls = rolls + 3
  put (p1, newP2, newdie, newRolls)
  return $ score p1 newRolls

playGame1 :: (Player, Player) -> Occur -> Games
playGame1 (p1, p2) numUniv =
  foldOccur
    (\roll numRoll games ->
       insertMany (advance p1 roll, p2) (numUniv * numRoll) games)
    empty
    dirac

playGame2 :: (Player, Player) -> Occur -> Games
playGame2 (p1, p2) numUniv =
  foldOccur
    (\roll numRoll games ->
       insertMany (p1, advance p2 roll) (numUniv * numRoll) games)
    empty
    dirac

doGame1 :: Game2
doGame1 = do
  universes <- get
  let played = foldOccur (\a o b -> playGame1 a o `union` b) empty universes
      (won, undecided) = partition p1Wins played
      wins = size won
  put undecided
  return (V2 wins 0)

doGame2 :: Game2
doGame2 = do
  universes <- get
  let played = foldOccur (\a o b -> playGame2 a o `union` b) empty universes
      (won, undecided) = partition p2Wins played
      wins = size won
  put undecided
  return (V2 0 wins)

p1Wins :: (Player, Player) -> Bool
p1Wins (Player _ score, _) = score >= 21

p2Wins :: (Player, Player) -> Bool
p2Wins (_, Player _ score) = score >= 21

score :: Player -> Rolls -> Int
score (Player _ score) rolls = score * rolls

best :: Win -> Int
best (V2 a b) = max a b

game1 :: Game1
game1 = do
  s1 <- advanceP1
  (Player _ p1score, _, _, _) <- get
  s2 <- advanceP2
  curState@(_, Player _ p2score, _, _) <- get
  let result
        | p1score >= 1000 = s1
        | p2score >= 1000 = s2
        | otherwise = evalState game1 curState
  return result

game2 :: Game2
game2 = do
  p1Win <- doGame1
  p2Win <- doGame2
  universes <- get
  let interm = p1Win + p2Win
      result
        | null universes = interm
        | otherwise = interm + evalState game2 universes
  return result

toState :: String -> (Player, Player, Die, Rolls)
toState input = (Player p1 0, Player p2 0, 0, 0)
  where
    [[_, p1], [_, p2]] = integers input

toState2 :: String -> Games
toState2 input = singleton (p1, p2)
  where
    p1 = Player pos1 0
    p2 = Player pos2 0
    [[_, pos1], [_, pos2]] = integers input

part1 :: Bool -> String -> String
part1 _ = show . evalState game1 . toState

part2 :: Bool -> String -> String
part2 _ = show . best . evalState game2 . toState2
