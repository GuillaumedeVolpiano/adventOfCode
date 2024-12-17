module Day18
  ( part1
  , part2
  ) where

import           Data.Text      as T (Text, init, unpack)

expand :: [Bool] -> [Bool]
expand line = gol (False : line)

gol :: [Bool] -> [Bool]
gol [x, y] = [x]
gol (x:xs@(y:z:_)) =
  ((x && not y && not z)
     || (x && y && not z)
     || (not x && y && z)
     || (not x && not y && z))
    : gol xs

render :: [[Bool]] -> String
render =
  unlines
    . map
        (map
           (\x ->
              if x
                then '^'
                else '.'))

part1 :: Bool -> Text -> String
part1 test =
  show
    . sum
    . map (length . filter not)
    . take n
    . iterate expand
    . map (== '^')
    . unpack
    . T.init
  where
    n
      | test = 10
      | otherwise = 40

part2 :: Bool -> Text -> String
part2 _ =
  show
    . sum
    . map (length . filter not)
    . take 400000
    . iterate expand
    . map (== '^')
    . unpack
    . T.init
