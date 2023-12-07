module Day2 (part1, part2) where 
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Bifunctor     (second)

type Game = (Int, Round)

type Round = [(Int, Colour)]

type Colour = String

parseLine :: String -> Game
parseLine s = (index, round)
  where
    index = read . init $ (s =~ "[0-9]+:" :: String)
    draw = getAllTextMatches (s =~ "[0-9]+ [a-z]+") :: [String]
    round = map (\a -> (read (a =~ "[0-9]+"), a =~ "[a-z]+")) draw

power :: Game -> Int
power (_, round) = red * green * blue
  where
    red = only "red"
    green = only "green"
    blue = only "blue"
    only c = maximum . map fst . filter (\p -> snd p == c) $ round

draws :: String -> [Game]
draws = map parseLine . lines

part1 :: Bool -> String -> String
part1 _ = show .    sum .
    map fst .
    filter (null . snd) .
    map
      (second
         (filter
            (\(c, d) ->
               (d == "red" && c > 12) ||
               (d == "green" && c > 13) || (d == "blue" && c > 14)))) .
    draws

part2 :: Bool -> String -> String
part2 _ = show . sum . map power . draws
