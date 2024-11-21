module Day3
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.Set             (Set, empty, fromList, intersection, size,
                                       union)
import           Helpers.Graph        (Pos)
import           Helpers.Parsers      (Parser)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      (eof, many, optional, parse, takeWhile1P)
import           Text.Megaparsec.Char (char, eol, string)

type Claim = (Id, Rec)

type Rec = Set Pos

type Id = Int

findUnclaimed :: [Claim] -> Int
findUnclaimed claims =
  fst . head . filter (null . intersection overclaimed . snd) $ claims
  where
    overclaimed = findOverlaps claims

findOverlaps :: [Claim] -> Set Pos
findOverlaps =
  snd .
  foldr
    (\(_, a) (claimed, overclaimed) ->
       (a `union` claimed, intersection a claimed `union` overclaimed))
    (empty, empty)

parser :: Parser [Claim]
parser = many parseLine <* eof

parseLine :: Parser Claim
parseLine = do
  void . char $ '#'
  iD <- parseInt
  void . string $ " @ "
  ox <- parseInt
  void . char $ ','
  oy <- parseInt
  void . string $ ": "
  width <- parseInt
  void . char $ 'x'
  height <- parseInt
  void . optional $ eol
  return
    ( iD
    , fromList
        [V2 x y | x <- [ox .. ox + width - 1], y <- [oy .. oy + height - 1]])

parseInt :: Parser Int
parseInt = read <$> takeWhile1P Nothing isDigit

part1 :: Bool -> String -> String
part1 _ = show . size . findOverlaps . fromRight [] . parse parser ""

part2 :: Bool -> String -> String
part2 _ = show . findUnclaimed . fromRight [] . parse parser ""
