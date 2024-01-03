module Day25
  ( part1
  , part2
  ) where

import           Data.HashMap.Strict as M (HashMap, fromList, mapKeys, member,
                                           (!))
import           Data.List           (unfoldr)
import           Linear.V2           (V2 (..))

data Cucumber
  = East
  | South
  deriving (Show, Eq, Ord)

type Pos = V2 Int

type Trench = HashMap Pos Cucumber

type Bounds = Pos

south = V2 0 1

east = V2 1 0

moveCucumberEast :: Bounds -> Pos -> Pos
moveCucumberEast bounds p = vMod (p + east) bounds

moveCucumberSouth :: Bounds -> Pos -> Pos
moveCucumberSouth bounds p = vMod (p + south) bounds

vMod :: Pos -> Bounds -> Pos
vMod (V2 x y) (V2 mx my) = V2 (mod x mx) (mod y my)

isEast :: Cucumber -> Bool
isEast East = True
isEast _    = False

isSouth :: Cucumber -> Bool
isSouth = not . isEast

moveEast :: Trench -> Bounds -> Pos -> Pos
moveEast trench bounds pos
  | isEast (trench ! pos) && not (moved `member` trench) = moved
  | otherwise = pos
  where
    moved = moveCucumberEast bounds pos

moveSouth :: Trench -> Bounds -> Pos -> Pos
moveSouth trench bounds pos
  | isSouth (trench ! pos) && not (moved `member` trench) = moved
  | otherwise = pos
  where
    moved = moveCucumberSouth bounds pos

moveTrench :: Bounds -> Trench -> Trench
moveTrench bounds trench = mapKeys (moveSouth eastMove bounds) eastMove
  where
    eastMove = mapKeys (moveEast trench bounds) trench

mapTrench :: [String] -> (Trench, Bounds)
mapTrench input =
  ( fromList
      [ toCucumber (input !! y !! x) x y
      | x <- [0 .. width - 1]
      , y <- [0 .. height - 1]
      , input !! y !! x /= '.'
      ]
  , V2 width height)
  where
    width = length . head $ input
    height = length input
    toCucumber '>' x y = (V2 x y, East)
    toCucumber 'v' x y = (V2 x y, South)

moving :: Bounds -> Trench -> Maybe (Trench, Trench)
moving bounds trench
  | moved == trench = Nothing
  | otherwise = Just (moved, moved)
  where
    moved = moveTrench bounds trench

doMoves :: (Trench, Bounds) -> [Trench]
doMoves (trench, bounds) = unfoldr (moving bounds) trench

part1 :: Bool -> String -> String
part1 _ = show . (+ 1) . length . doMoves . mapTrench . lines

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
