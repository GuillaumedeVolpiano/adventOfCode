module Day17
  ( part1
  , part2
  ) where

import           Data.List     as L (foldl')
import           Data.Maybe    (fromJust)
import           Data.Sequence as S (Seq, index, insertAt, length, singleton)

short = [1 .. 2017]

long = [1 .. 5 * 10 ^ 7]

data State = State
  { step   :: Step
  , pos    :: Pos
  , memory :: Memory
  } deriving (Show)

data QuickState = QuickState
  { qStep :: Step
  , qPos  :: Pos
  , p1    :: Maybe Int
  } deriving (Show)

type Step = Int

type Pos = Int

type Memory = Seq Int

doStep :: State -> Int -> State
doStep state p = state {pos = pos', memory = insertAt pos' p . memory $ state}
  where
    pos' = 1 + (pos state + step state) `mod` p

doQuickStep :: QuickState -> Int -> QuickState
doQuickStep state p
  | pos' == 1 = state {qPos = pos', p1 = Just p}
  | otherwise = state {qPos = pos'}
  where
    pos' = 1 + (qPos state + qStep state) `mod` p

initialState :: Int -> State
initialState step = State step 0 . singleton $ 0

initialQuickState :: Int -> QuickState
initialQuickState step = QuickState step 0 Nothing

nextVal :: State -> Int
nextVal (State _ curPos seq) = index seq curPos'
  where
    curPos' = (curPos + 1) `mod` S.length seq

part1 :: Bool -> String -> String
part1 _ input = show . nextVal . foldl' doStep state $ short
  where
    state = initialState . read $ input

part2 :: Bool -> String -> String
part2 _ input = show . fromJust . p1 . foldl' doQuickStep quickState $ long
  where
    quickState = initialQuickState . read $ input
