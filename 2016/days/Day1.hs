module Day1
  ( part1
  , part2
  ) where

import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.Set             (Set, empty, insert, member)
import           Helpers.Graph        (Pos, left, manhattanDistance, north,
                                       origin, right)
import           Helpers.Parsers      (Parser)
import           Linear.Vector        ((*^))
import           Text.Megaparsec      (optional, parse, takeWhileP, (<|>))
import           Text.Megaparsec.Char (eol, string, upperChar)

type Dir = Pos

readDir :: Char -> (Dir -> Dir)
readDir 'R' = right
readDir 'L' = left

parseInput :: Pos -> Dir -> Parser Int
parseInput pos dir = do
  turn <- readDir <$> upperChar
  dist <- read <$> takeWhileP Nothing isDigit
  optional . string $ ", "
  let dir' = turn dir
      pos' = pos + dist *^ dir'
  end pos' <|> parseInput pos' dir'

parseTwice :: Pos -> Dir -> Set Pos -> Parser Int
parseTwice pos dir seen = do
  turn <- readDir <$> upperChar
  dist <- read <$> takeWhileP Nothing isDigit
  optional . string $ ", "
  let dir' = turn dir
      pos' = pos + dist *^ dir'
      seen' = insert pos' seen
      result
        | pos' `member` seen = return . manhattanDistance origin $ pos'
        | otherwise = parseTwice pos' dir' seen'
  result

end :: Pos -> Parser Int
end pos = do
  eol
  return $ manhattanDistance origin pos

part1 :: Bool -> String -> String
part1 _ = show . fromRight 0 . parse (parseInput origin north) ""

part2 :: Bool -> String -> String
part2 _ = show . fromRight 0 . parse (parseTwice origin north empty) ""
