module Day22
  ( part1
  , part2
  ) where

import           Data.Array.IArray (assocs)
import           Data.Map          (Map, findWithDefault, fromList, insert,
                                    keys)
import           Helpers.Parsers   (arrayFromString)
import           Linear.V2         (V2 (..))

data Carrier =
  Carrier Pos Pos Int
  deriving (Show)

data Node
  = Clean
  | Weakened
  | Infected
  | Flagged
  deriving (Show, Eq)

type Pos = V2 Int

type Grid = Map Pos Node

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

uturn :: Pos -> Pos
uturn (V2 x y) = V2 (-x) (-y)

start :: Grid -> (Carrier, Grid)
start grid = (Carrier startPos (V2 0 (-1)) 0, grid)
  where
    k = keys grid
    maxX = maximum . map (\(V2 x _) -> x) $ k
    minX = minimum . map (\(V2 x _) -> x) $ k
    maxY = maximum . map (\(V2 _ y) -> y) $ k
    minY = minimum . map (\(V2 _ y) -> y) $ k
    startPos = V2 (div (maxX - minX) 2) (div (maxY - minY) 2)

explore :: (Carrier, Grid) -> (Carrier, Grid)
explore (Carrier pos dir infected, grid) = (Carrier pos' dir' infected', grid')
  where
    curNode = findWithDefault Clean pos grid
    dir'
      | curNode == Infected = right dir
      | curNode == Clean = left dir
    toggle
      | curNode == Infected = Clean
      | curNode == Clean = Infected
    grid' = insert pos toggle grid
    pos' = pos + dir'
    infected'
      | curNode == Clean = infected + 1
      | otherwise = infected

evolved :: (Carrier, Grid) -> (Carrier, Grid)
evolved (Carrier pos dir infected, grid) = (Carrier pos' dir' infected', grid')
  where
    curNode = findWithDefault Clean pos grid
    dir'
      | curNode == Clean = left dir
      | curNode == Weakened = dir
      | curNode == Infected = right dir
      | curNode == Flagged = uturn dir
    toggle
      | curNode == Clean = Weakened
      | curNode == Weakened = Infected
      | curNode == Infected = Flagged
      | curNode == Flagged = Clean
    grid' = insert pos toggle grid
    pos' = pos + dir'
    infected'
      | curNode == Weakened = infected + 1
      | otherwise = infected

infected :: Carrier -> Int
infected (Carrier _ _ i) = i

part1 :: Bool -> String -> String
part1 _ =
  show
    . infected
    . fst
    . (!! 10000)
    . iterate explore
    . start
    . fmap
        (\x ->
           if x == '#'
             then Infected
             else Clean)
    . fromList
    . assocs
    . arrayFromString

part2 :: Bool -> String -> String
part2 _ =
  show
    . infected
    . fst
    . (!! 10000000)
    . iterate evolved
    . start
    . fmap
        (\x ->
           if x == '#'
             then Infected
             else Clean)
    . fromList
    . assocs
    . arrayFromString
