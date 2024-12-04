module Day4
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, (!?))
import           Helpers.Graph      (Pos, dirs, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Linear.Vector      ((*^))

type Grid = UArray Pos Char

compass =
  (north + east) : (north + west) : (south + east) : (south + west) : dirs

findXs :: Grid -> [Pos]
findXs = map fst . filter ((== 'X') . snd) . assocs

findAs :: Grid -> [Pos]
findAs = map fst . filter ((== 'A') . snd) . assocs

findXMAS :: Grid -> Pos -> Int
findXMAS grid p =
  length
    . filter (== [Just 'X', Just 'M', Just 'A', Just 'S'])
    . map (\d -> map (\s -> grid !? (p + (s *^ d))) [0 .. 3])
    $ compass

findXedMAS :: Grid -> Pos -> Bool
findXedMAS grid p = nesw `elem` mas && nwse `elem` mas
  where
    mas = [[Just 'M', Just 'A', Just 'S'], [Just 'S', Just 'A', Just 'M']]
    nesw = map (grid !?) [p + north + east, p, p + south + west]
    nwse = map (grid !?) [p + north + west, p, p + south + east]

findAll :: Grid -> Int
findAll grid = sum . map (findXMAS grid) . findXs $ grid

findAllXed :: Grid -> Int
findAllXed grid = length . filter (findXedMAS grid) . findAs $ grid

part1 :: Bool -> String -> String
part1 _ = show . findAll . arrayFromString

part2 :: Bool -> String -> String
part2 _ = show . findAllXed . arrayFromString
