module Day17
  ( part1
  , part2
  ) where

import           Text.Regex.TDFA ((=~))

import           Data.List.Split (chunksOf)
import           Data.Map        as M (Map, empty, findWithDefault, insert,
                                       keys, lookup)
import           Data.Maybe      (Maybe (Just, Nothing), fromJust, isNothing)
import           Data.Sequence   (fromList)
import           Helpers.Search  (findPattern)
import           Linear.V2       (V2 (..))

type Pos = V2 Int

type Cave = Map Pos Bool

type Height = Int

type Rocks = [[Pos]]

type Jets = [Pos]

data State =
  State
    { cave   :: Cave
    , height :: Height
    , jets   :: Jets
    , rocks  :: Rocks
    }
  deriving (Show, Eq)

initialRocks =
  cycle
    [ [V2 x 0 | x <- [0 .. 3]]
    , [V2 x y | x <- [0 .. 2], y <- [0 .. 2], x == 1 || y == 1]
    , [V2 x y | x <- [0 .. 2], y <- [0 .. 2], x == 2 || y == 0]
    , [V2 0 y | y <- [0 .. 3]]
    , [V2 x y | x <- [0, 1], y <- [0, 1]]
    ]

lottaRocks = 1000000000000

fallRock :: State -> State
fallRock state = State newCave newHeight newJets rs
  where
    (rock:rs) = rocks state
    startPos = map (\a -> a + V2 2 (3 + height state)) rock
    curCave = cave state
    newCave = foldl (\a b -> insert b True a) curCave movedRock
    newHeight =
      max (height state) ((maximum . map (\(V2 _ y) -> y) $ movedRock) + 1)
    (movedRock, newJets) = jetFall (startPos, jets state)
    jetFall (s, j:js)
      | isNothing . fall . jet j $ s = (jet j s, js)
      | otherwise = jetFall (fromJust . fall . jet j $ s, js)
    jet j s
      | canjet = jetted
      | otherwise = s
      where
        jetted = map (+ j) s
        canjet =
          all (\(V2 x _) -> x >= 0) jetted &&
          all (\(V2 x _) -> x <= 6) jetted &&
          all (\p -> isNothing . M.lookup p $ curCave) jetted
    fall s
      | canFall = Just fell
      | otherwise = Nothing
      where
        fell = map (+ V2 0 (-1)) s
        canFall =
          all (\p -> isNothing . M.lookup p $ curCave) fell &&
          all (\(V2 _ y) -> y >= 0) fell

patternHeight :: [State] -> Int -> Int
patternHeight states patLength =
  height (states !! (1000 + patLength)) - height (states !! 1000)

predictHeight :: [State] -> Int -> Int -> Int
predictHeight states jetLength numRocks = prediction
  where
    patL =
      findPattern
        1000
        1
        (\a b ->
           (head . rocks $ a) == (head . rocks $ b) &&
           take jetLength (jets a) == take jetLength (jets b)) $
      fromList states
    patH = patternHeight states patL
    toFall = numRocks - 1000
    (times, remainder) = divMod toFall patL
    supp = height (states !! (1000 + remainder))
    prediction = times * patH + supp

jetsList :: String -> [Pos]
jetsList input =
  cycle .
  map
    (\x ->
       if x == '<'
         then V2 (-1) 0
         else V2 1 0) $
  (input =~ "[<>]+")

part1 :: Bool -> String -> String
part1 _ input =
  show . height . last . take 2023 . iterate fallRock $ initialState
  where
    initialState = State empty 0 (jetsList input) initialRocks

part2 :: Bool -> String -> String
part2 _ input = show $ predictHeight alot jetCycle lottaRocks
  where
    alot = take 10000 . iterate fallRock $ initialState
    initialState = State empty 0 (jetsList input) initialRocks
    jetCycle = length (input =~ "[<>]+" :: String)
