module Day2
  ( part1
  , part2
  ) where

import           Parsers        (characters, integers)

import           Data.Bifunctor (second)

type Game = (Int, Round)

type Round = [(Int, Colour)]

type Colour = String

parseLines :: String -> [Game]
parseLines s = 
  map (\((a, _):b) -> (a, b)) . 
        zipWith zip (integers s) $ characters s

power :: Game -> Int
power (_, round) = red * green * blue
  where
    red = only "red"
    green = only "green"
    blue = only "blue"
    only c = maximum . map fst . filter (\p -> snd p == c) $ round

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  map fst .
  filter (null . snd) .
  map
    (second
       (filter
          (\(c, d) ->
             (d == "red" && c > 12) ||
             (d == "green" && c > 13) || (d == "blue" && c > 14)))) .
 parseLines 

part2 :: Bool -> String -> String
part2 _ = show . sum . map power . parseLines
