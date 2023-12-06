import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
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

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let draws = map parseLine . lines $ input
  putStrLn "part 1"
  print .
    sum .
    map fst .
    filter (null . snd) .
    map
      (second
         (filter
            (\(c, d) ->
               (d == "red" && c > 12) ||
               (d == "green" && c > 13) || (d == "blue" && c > 14)))) $
    draws
  putStrLn "part 2"
  print . sum . map power $ draws
