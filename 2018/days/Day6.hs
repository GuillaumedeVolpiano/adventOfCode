module Day6
  ( part1
  , part2
  ) where

import           Control.Lens.Getter  ((^.))
import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.List            (group, sort, sortBy)
import           Data.Maybe           (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Graph        (Pos)
import           Helpers.Parsers      (Parser)
import           Linear.V2            (V2 (..), _x, _y)
import           Text.Megaparsec      (eof, many, optional, parse, takeWhile1P)
import           Text.Megaparsec.Char (char, eol, string)

parser :: Parser [Pos]
parser = many parseLine <* eof

parseLine :: Parser Pos
parseLine = do
  x <- read <$> takeWhile1P Nothing isDigit
  void . string $ ", "
  y <- read <$> takeWhile1P Nothing isDigit
  void . optional $ eol
  return (V2 x y)

manDist :: Pos -> Pos -> Int
manDist (V2 a b) (V2 c d) = abs (a - c) + abs (b - d)

largestArea :: [Pos] -> Int
largestArea coords =
  maximum .
  map length .
  group .
  sort .
  filter (\(V2 x y) -> x `notElem` [xm, xM] && y `notElem` [ym, yM]) .
  mapMaybe (closest coords) $
  [V2 x y | x <- [xm .. xM], y <- [ym .. yM]]
  where
    xs = map (^. _x) coords
    ys = map (^. _y) coords
    xm = minimum xs
    xM = maximum xs
    ym = minimum ys
    yM = maximum ys

safest :: Int -> [Pos] -> Int
safest dist coords =
  length . filter (\p -> (< dist) . sum . map (manDist p) $ coords) $
  [V2 x y | x <- [xm .. xM], y <- [ym .. yM]]
  where
    xs = map (^. _x) coords
    ys = map (^. _y) coords
    xm = minimum xs
    xM = maximum xs
    ym = minimum ys
    yM = maximum ys

closest :: [Pos] -> Pos -> Maybe Pos
closest coords pos
  | manDist pos s == manDist pos o = Nothing
  | otherwise = Just s
  where
    (s:o:_) = sortBy (\a b -> compare (manDist pos a) (manDist pos b)) coords

part1 :: Bool -> String -> String
part1 _ = show . largestArea . fromRight [] . parse parser ""

part2 :: Bool -> String -> String
part2 test = show . safest dist . fromRight [] . parse parser ""
  where
    dist
      | test = 32
      | otherwise = 10000
