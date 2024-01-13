module Day3
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.HashMap.Lazy    (HashMap, empty, fromList, intersection,
                                       keys, union, (!))
import           Data.List            (intersect)
import           Helpers.Graph        (Pos)
import           Helpers.Parsers      (Parser)
import           Linear.V2            (V2 (..))
import           Linear.Vector        ((*^))
import           Text.Megaparsec      (eof, optional, parse, takeWhile1P, try,
                                       (<|>))
import           Text.Megaparsec.Char (char)

type Wire = HashMap Pos Int

right = V2 1 0

left = V2 (-1) 0

up = V2 0 (-1)

down = V2 0 1

parseLine :: (Pos, Int) -> Parser Wire
parseLine pos =
  try (parseRight pos) <|> try (parseLeft pos) <|> try (parseUp pos) <|>
  try (parseDown pos) <|>
  parseEnd

parseRight :: (Pos, Int) -> Parser Wire
parseRight (pos, dist) = do
  void . char $ 'R'
  num <- read <$> takeWhile1P Nothing isDigit
  let curBranch =
        fromList . take num . map (\x -> (pos + x *^ right, dist + x)) $ [1 ..]
  void . optional . char $ ','
  rest <- parseLine (pos + num *^ right, dist + num)
  return (curBranch `union` rest)

parseLeft :: (Pos, Int) -> Parser Wire
parseLeft (pos, dist) = do
  void . char $ 'L'
  num <- read <$> takeWhile1P Nothing isDigit
  let curBranch =
        fromList . take num . map (\x -> (pos + x *^ left, dist + x)) $ [1 ..]
  void . optional . char $ ','
  rest <- parseLine (pos + num *^ left, dist + num)
  return (curBranch `union` rest)

parseUp :: (Pos, Int) -> Parser Wire
parseUp (pos, dist) = do
  void . char $ 'U'
  num <- read <$> takeWhile1P Nothing isDigit
  let curBranch =
        fromList . take num . map (\x -> (pos + x *^ up, dist + x)) $ [1 ..]
  void . optional . char $ ','
  rest <- parseLine (pos + num *^ up, dist + num)
  return (curBranch `union` rest)

parseDown :: (Pos, Int) -> Parser Wire
parseDown (pos, dist) = do
  void . char $ 'D'
  num <- read <$> takeWhile1P Nothing isDigit
  let curBranch =
        fromList . take num . map (\x -> (pos + x *^ down, dist + x)) $ [1 ..]
  void . optional . char $ ','
  rest <- parseLine (pos + num *^ down, dist + num)
  return (curBranch `union` rest)

parseEnd :: Parser Wire
parseEnd = do
  eof
  return empty

findIntersections :: [Wire] -> [Pos]
findIntersections [a, b] = keys $ a `intersection` b

nearest :: [Wire] -> Int
nearest = minimum . map (\(V2 x y) -> abs x + abs y) . findIntersections

closest :: [Wire] -> Int
closest [a, b] =
  minimum . map (\x -> a ! x + b ! x) . findIntersections $ [a, b]

part1 :: Bool -> String -> String
part1 _ =
  show .
  nearest . map (fromRight empty . parse (parseLine (V2 0 0, 0)) "") . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  closest . map (fromRight empty . parse (parseLine (V2 0 0, 0)) "") . lines
