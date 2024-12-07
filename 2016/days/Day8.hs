module Day8
  ( part1
  , part2
  ) where

import           Data.Char             (isDigit)
import           Data.Either           (fromRight)
import           Data.Set              as S (Set, delete, empty, filter, insert,
                                             map, member, size)
import           Data.Text             (Text)
import           Helpers.Graph         (Pos)
import           Helpers.Parsers.Text  (Parser, decimal, string)
import           Linear.V2             (V2 (..))
import           Text.Megaparsec       (eof, optional, parse, takeWhileP, try,
                                        (<|>))
import           Text.Megaparsec.Char  (char, eol)

type Screen = Set Pos

rows test
  | test = 3
  | otherwise = 6

columns test
  | test = 7
  | otherwise = 50

parseInput :: Bool -> Screen -> Parser Screen
parseInput test screen =
  try (parseRect test screen)
    <|> try (parseRotateR test screen)
    <|> parseRotateC test screen
    <|> end screen

end :: Screen -> Parser Screen
end screen = do
  eof
  return screen

parseRect :: Bool -> Screen -> Parser Screen
parseRect test screen = do
  string "rect "
  a <- decimal
  char 'x'
  b <- decimal
  optional eol
  parseInput test . rect a b $ screen

parseRotateR :: Bool -> Screen -> Parser Screen
parseRotateR test screen = do
  string "rotate row y="
  index <- decimal
  string "by "
  shift <- decimal
  optional eol
  parseInput test . rotateR test index shift $ screen

parseRotateC :: Bool -> Screen -> Parser Screen
parseRotateC test screen = do
  string "rotate column x="
  index <- decimal
  string "by "
  shift <- decimal
  optional eol
  parseInput test . rotateC test index shift $ screen

rect :: Int -> Int -> Screen -> Screen
rect a b screen =
  foldr insert screen [V2 x y | x <- [0 .. (a - 1)], y <- [0 .. b - 1]]

rotateR :: Bool -> Int -> Int -> Screen -> Screen
rotateR test index shift screen = foldr insert screen' row'
  where
    row = S.filter (\(V2 _ y) -> y == index) screen
    row' = S.map (\(V2 x y) -> V2 (mod (x + shift) (columns test)) y) row
    screen' = foldr delete screen row

rotateC :: Bool -> Int -> Int -> Screen -> Screen
rotateC test index shift screen = foldr insert screen' column'
  where
    column = S.filter (\(V2 x _) -> x == index) screen
    column' = S.map (\(V2 x y) -> V2 x (mod (y + shift) (rows test))) column
    screen' = foldr delete screen column

render :: Bool -> Screen -> String
render test screen = unlines . fmap line $ [0 .. rows test - 1]
  where
    line y = [point (V2 x y) | x <- [0 .. columns test - 1]]
    point p
      | p `member` screen = '#'
      | otherwise = ' '

part1 :: Bool -> Text -> String
part1 test =
  show . size . fromRight empty . parse (parseInput test empty) ""

part2 :: Bool -> Text -> String
part2 test = render test . fromRight empty . parse (parseInput test empty) ""
