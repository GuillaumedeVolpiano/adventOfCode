module Day25
  ( part1
  , part2
  ) where

import           Data.Set    as St (Set, fromList, map, notMember)
import           Linear.V2   (V2 (..))

import           Debug.Trace

data Cucumber
  = East Pos Pos
  | South Pos Pos
  deriving (Show, Eq, Ord)

type Pos = V2 Int

type Trench = Set Cucumber

south = V2 0 1

east = V2 1 0

pos :: Cucumber -> Pos
pos (East p _)  = p
pos (South p _) = p

moveCucumber :: Cucumber -> Cucumber
moveCucumber (East p v)  = East (vMod (p + east) v) v
moveCucumber (South p v) = South (vMod (p + south) v) v

vMod :: Pos -> Pos -> Pos
vMod (V2 x y) (V2 mx my) = V2 (mod x mx) (mod y my)

isEast :: Cucumber -> Bool
isEast (East _ _) = True
isEast _          = False

isSouth :: Cucumber -> Bool
isSouth = not . isEast

moveEast :: Set Pos -> Cucumber -> Cucumber
moveEast posTrench cucumber
  | isEast cucumber && pos moved `notMember` posTrench = moved
  | otherwise = cucumber
  where
    moved = moveCucumber cucumber

moveSouth :: Set Pos -> Cucumber -> Cucumber
moveSouth posTrench cucumber
  | isSouth cucumber && pos moved `notMember` posTrench = moved
  | otherwise = cucumber
  where
    moved = moveCucumber cucumber

moveTrench :: Int -> Trench -> Int
moveTrench acc trench
  | trench == moved = acc
  | otherwise = moveTrench (acc + 1) moved
  where
    movedEast = St.map (moveEast posTrench) trench
    moved = St.map (moveSouth posEast) movedEast
    posTrench = St.map pos trench
    posEast = St.map pos movedEast

mapTrench :: [String] -> Trench
mapTrench input =
  fromList
    [ toCucumber (input !! y !! x) x y
    | x <- [0 .. width - 1]
    , y <- [0 .. height - 1]
    , input !! y !! x /= '.'
    ]
  where
    width = length . head $ input
    height = length input
    toCucumber '>' x y = East (V2 x y) (V2 width height)
    toCucumber 'v' x y = South (V2 x y) (V2 width height)

part1 :: Bool -> String -> String
part1 _ = show . moveTrench 1 . mapTrench . lines

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
