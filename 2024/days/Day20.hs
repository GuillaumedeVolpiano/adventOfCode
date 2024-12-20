module Day20
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, array, (!?))
import           Data.Bits          (shiftR, (.&.))
import           Data.IntMap        as M (member, singleton, (!))
import           Data.List          (group, minimumBy, sort)
import           Data.Maybe         (mapMaybe)
import           Data.Sequence      as Sq (singleton)
import           Data.Text          (Text, index)
import qualified Data.Text          as T (length, lines)
import           Helpers.Search.Int (bfsAll)

type Pos = Int

type Maze = UArray Pos Char

origin = 0

dirs = [1, -1, 256, -256]

manhattanDistance :: Int -> Int -> Int
manhattanDistance a b =
  abs ((a .&. 255) - (b .&. 255)) + abs (shiftR a 8 - shiftR b 8)

countCheats :: Bool -> Int -> Text -> Int
countCheats test cheatLast input = length cheated
  where
    assocsMaze =
      [(x + 256 * y, l !! y `index` x) | x <- [0 .. width], y <- [0 .. height]]
    l = T.lines input
    height = length l - 1
    width = (-1 +) . T.length . head $ l
    maze = array (origin, width + 256 * height) assocsMaze
    start = fst . head . filter ((== 'S') . snd) $ assocsMaze
    end = fst . head . filter ((== 'E') . snd) $ assocsMaze
    refTime = fromStart ! end
    walkable = map fst . filter ((== '.') . snd) $ assocsMaze
    fromStart =
      bfsAll (Sq.singleton start) (M.singleton start 0) (neighbours maze)
    noCollision pos =
      map (\p -> (pos, cheatDist pos p))
        . filter
            (\p ->
               manhattanDistance p pos > 1
                 && manhattanDistance p pos <= cheatLast
                 && cheatDist pos p >= threshold)
        $ (end : walkable)
    cheatFrom pos
      | null . noCollision $ pos = Nothing
      | otherwise = Just . noCollision $ pos
    cheated = concat . mapMaybe cheatFrom $ (start : walkable)
    cheatDist p1 p2 = fromStart ! p2 - fromStart ! p1 - manhattanDistance p1 p2
    threshold
      | test = 1
      | otherwise = 100

neighbours :: Maze -> Pos -> [Pos]
neighbours maze pos =
  filter (\p -> maze !? p `elem` [Just '.', Just 'E']) . map (pos +) $ dirs

part1 :: Bool -> Text -> String
part1 test = show . countCheats test 2

part2 :: Bool -> Text -> String
part2 test = show . countCheats test 20
