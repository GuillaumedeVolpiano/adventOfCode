module Day24
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Either          (fromRight)
import           Data.HashSet         as St (HashSet, delete, difference, empty,
                                             filter, fromList, insert,
                                             intersection, map, member, size,
                                             union)
import           Data.List            as L (map)
import           Data.Maybe           (fromJust, isNothing)
import           Helpers.Parsers      (Parser, Pos)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      (count, eof, parse, try, (<|>))
import           Text.Megaparsec.Char (char, letterChar, string)

type Floor = HashSet Pos

directions =
  [ ("e", V2 2 0)
  , ("w", V2 (-2) 0)
  , ("ne", V2 1 (-1))
  , ("se", V2 1 1)
  , ("nw", V2 (-1) (-1))
  , ("sw", V2 (-1) 1)
  ]

neighbSeed = fromList . L.map snd $ directions

parseFloor :: Floor -> String -> Floor
parseFloor floor = fromRight empty . parse (parseTile floor) ""

parseTile :: Floor -> Parser Floor
parseTile floor = do
  r <- parseDir
  let result
        | r `member` floor = delete r floor
        | otherwise = insert r floor
  return result

parseDir :: Parser Pos
parseDir = try e <|> try w <|> try two <|> end
  where
    e = do
      void . char $ 'e'
      r <- parseDir
      return (V2 2 0 + r)
    w = do
      void . char $ 'w'
      r <- parseDir
      return (V2 (-2) 0 + r)
    two = do
      key <- count 2 letterChar
      let d = fromJust . lookup key $ directions
      r <- parseDir
      return (d + r)
    end = do
      eof
      return (V2 0 0)

tile :: Pos -> Floor -> String -> Floor
tile pos floor s
  | null s = nFloor
  | otherwise = tile nPos floor rest
  where
    (cur, rest)
      | head s == 'e' || head s == 'w' = splitAt 1 s
      | otherwise = splitAt 2 s
    nPos = (+ pos) . fromJust . lookup cur $ directions
    nFloor
      | member pos floor = delete pos floor
      | otherwise = insert pos floor

gol :: Floor -> Floor
gol floor = stillAlive `union` reborn
  where
    stillAlive = St.filter ((`elem` [1, 2]) . size . livingNeighbours) floor
    allNeighbours = foldl union empty . St.map neighbours $ floor
    notAlive = difference allNeighbours floor
    reborn = St.filter ((== 2) . size . livingNeighbours) notAlive
    livingNeighbours = intersection floor . neighbours
    neighbours x = St.map (x +) neighbSeed

tileFloor :: String -> Floor
tileFloor = foldl parseFloor empty . lines

part1 :: Bool -> String -> String
part1 _ = show . size . tileFloor

part2 :: Bool -> String -> String
part2 _ = show . size . last . take 101 . iterate gol . tileFloor
