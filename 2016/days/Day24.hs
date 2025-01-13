{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Control.Monad.State  (State, evalState, get, modify, put)
import           Data.Bifunctor       (bimap, second)
import           Data.Bits            (shiftL, shiftR, (.&.))
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.IntMap.Strict   (IntMap, assocs, elems, filterWithKey,
                                       keys, (!))
import qualified Data.IntMap.Strict   as M (empty, fromList, insert)
import           Data.IntSet          (IntSet, delete, findMin, member, size,
                                       toList)
import qualified Data.IntSet          as S (empty, foldr, fromList, insert)
import           Data.List            (sortBy, subsequences, tails)
import           Data.Maybe           (catMaybes)
import           Data.Ord             (Down (..), comparing)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Helpers.Search.Int   (bfsSafeDist, travelingSalesman,
                                       travelingSalesmanNoReturn)
import           Text.Megaparsec      (ParsecT, eof, runParserT, (<|>))
import           Text.Megaparsec.Char (char, eol, numberChar)

type Parser = ParsecT Void Text (State Int) (EdgePos, Maze)

type EdgePos = IntMap Int

type Maze = IntSet

type Edges = IntMap Int

type MultiCities = IntMap Int

type BestSubsets = IntMap Int

type Subset = IntSet

parseInput :: Parser
parseInput = do
  (parseWall <|> parsePath <|> parsePOI <|> parseEOL)
    <|> (eof >> return (M.empty, S.empty))

parseWall :: Parser
parseWall = do
  char '#'
  modify (+ 1)
  parseInput

parsePath :: Parser
parsePath = do
  char '.'
  pos <- get
  put (pos + 1)
  second (S.insert pos) <$> parseInput

parsePOI :: Parser
parsePOI = do
  poi <- digitToInt <$> numberChar
  pos <- get
  put (pos + 1)
  bimap (M.insert poi pos) (S.insert pos) <$> parseInput

parseEOL :: Parser
parseEOL = do
  eol
  modify ((.&. 65280) . (+ 256)) -- 2^16 - 2^8
  parseInput

findPaths :: (EdgePos, Maze) -> Edges
findPaths refs@(edgePos, _) = M.fromList edgePairs
  where
    edges = keys edgePos
    edgePairs =
      concatMap catMaybes . zipWith (map . findDist refs) edges . tails . tail
        $ edges

findDist :: (EdgePos, Maze) -> Int -> Int -> Maybe (Int, Int)
findDist (edgePos, maze) e1 e2 = (e2 + shiftL e1 3, ) <$> dist
  where
    from = edgePos ! e1
    to = edgePos ! e2
    dist = bfsSafeDist from neighbours (== to)
    neighbours pos = filter (`member` maze) . map (pos +) $ [1, -1, 256, -256]

part1 :: Bool -> Text -> String
part1 _ =
  show
    . minimum
    . elems
    . travelingSalesmanNoReturn 3
    . findPaths
    . fromRight (error "parser failed")
    . flip evalState 0
    . runParserT parseInput "day24"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . travelingSalesman 3
    . findPaths
    . fromRight (error "parser failed")
    . flip evalState 0
    . runParserT parseInput "day24"
