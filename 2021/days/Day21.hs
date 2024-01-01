module Day21
  ( part1
  , part2
  ) where

import           Control.Monad.State (State, evalState, get, gets, put)
import           Data.Hashable       (Hashable, hashWithSalt)
import           Data.HashPSQ        (HashPSQ, alter, minView, singleton)
import           Data.Maybe          (Maybe (Just, Nothing), fromJust,
                                      isNothing)
import           Helpers.Parsers     (integers)
import           Linear.V2           (V2 (..))

type Pos = Int

type Score = Int

data Player =
  Player Pos Score
  deriving (Show, Eq)

data Players =
  Players Player Player
  deriving (Show, Eq, Ord)

type Die = Int

type Rolls = Int

type Occurences = Int

type Win = V2 Int

type Turn = Int

type PosTurn = (Player, Player, Turn)

type GameQueue = HashPSQ PosTurn Players Occurences

type Game1 = State (Player, Player, Die, Rolls) Int

type Game2 = State GameQueue Win

instance Ord Player where
  compare (Player p1 s1) (Player p2 s2) = compare p1 p2 `mappend` compare s1 s2

instance Hashable Player where
  hashWithSalt salt (Player p s) = hashWithSalt salt (2000 * p + s)

dirac = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

advanceP1 :: Game1
advanceP1 = do
  (Player p1pos p1score, p2, die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newp1pos = mod (p1pos + roll - 1) 10 + 1
      p1 = Player newp1pos (p1score + newp1pos)
      newdie = mod (die + 2) 100 + 1
      newRolls = rolls + 3
  put (p1, p2, newdie, newRolls)
  return $ score p2 newRolls

advanceP2 :: Game1
advanceP2 = do
  (p1, Player p2pos p2score, die, rolls) <- get
  let roll = die + mod (die + 1) 100 + mod (die + 2) 100 + 3
      newp2pos = mod (p2pos + roll - 1) 10 + 1
      newdie = mod (die + 2) 100 + 1
      p2 = Player newp2pos (p2score + newp2pos)
      newRolls = rolls + 3
  put (p1, p2, newdie, newRolls)
  return $ score p1 newRolls

game2 :: Game2
game2 = do
  queue <- gets minView
  let result
        | isNothing queue = V2 0 0
        | otherwise = treat . fromJust $ queue
  return result

treat :: (PosTurn, Players, Occurences, GameQueue) -> Win
treat ((_, _, turn), Players p1@(Player _ s1) p2@(Player _ s2), oc, queue)
  | s1 >= 21 = V2 oc 0 + evalState game2 queue
  | s2 >= 21 = V2 0 oc + evalState game2 queue
  | otherwise = evalState game2 newQueue
  where
    newQueue =
      foldl
        (\a b@(newP1, newP2, _) ->
           snd . alter (updateQueue b) (newP1, newP2, newTurn) $ a)
        queue
        newPlayers
    newPlayers
      | turn == 1 = map (\(a, b) -> (a, p2, b)) . advanceBy $ p1
      | otherwise = map (\(a, b) -> (p1, a, b)) . advanceBy $ p2
    newTurn = 3 - turn
    advanceBy (Player pos score) =
      map (\(a, b) -> (updatePlayer pos score a, b * oc)) dirac
    updatePlayer pos score a = Player (newPos pos a) (score + newPos pos a)
    newPos pos a = mod (pos + a - 1) 10 + 1
    updateQueue (newP1, newP2, newOc) Nothing =
      (0, Just (Players newP1 newP2, newOc))
    updateQueue (newP1, newP2, newOc) (Just (_, oldOc)) =
      (0, Just (Players newP1 newP2, newOc + oldOc))

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

toState :: String -> (Player, Player, Die, Rolls)
toState input = (Player p1 0, Player p2 0, 0, 0)
  where
    [[_, p1], [_, p2]] = integers input

toState2 :: String -> HashPSQ PosTurn Players Occurences
toState2 input = singleton (p1, p2, 1) (Players p1 p2) 1
  where
    p1 = Player pos1 0
    p2 = Player pos2 0
    [[_, pos1], [_, pos2]] = integers input

part1 :: Bool -> String -> String
part1 _ = show . evalState game1 . toState

part2 :: Bool -> String -> String
part2 _ = show . best . evalState game2 . toState2
