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
import           Helpers.Search.Int   (bfsSafeDist)
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

-- given the form of the input, there are atmost 10 points of interest, and in
-- fact there are 8, numbered 1 to 7
-- We are going to encode the (set, k) as (s1*8^n ++ s2 * 8^(n-1) ++ sn * 8 + k)
travelingSalesmanNoReturn :: Edges -> BestSubsets
travelingSalesmanNoReturn edges =
  filterWithKey (\k _ -> k > 8 ^ 7) $ findSubsets
  where
    pois =
      map S.fromList . sortBy (comparing (Down . length)) . tail . subsequences
        $ [1 .. 7]
    findSubsets = foldr mapSubset M.empty pois
    mapSubset :: Subset -> BestSubsets -> BestSubsets
    mapSubset set subsets
      | size set == 1 =
        M.insert (encode set + findMin set) (edges ! findMin set) subsets
      | otherwise = S.foldr (subsetise set) subsets set
    subsetise :: Subset -> Int -> BestSubsets -> BestSubsets
    subsetise set k bests =
      M.insert (encode set + k) (bestSub bests k . delete k $ set) bests
    bestSub bests k set =
      minimum
        [bests ! (encode set + m) + edges ! edgeCode m k | m <- toList set]
    edgeCode m k = mMK + shiftL mmk 3
      where
        mmk = min m k
        mMK = max m k
    encode = flip shiftL 3 . S.foldr (\a b -> a + shiftL b 3) 0

travelingSalesman :: Edges -> Int
travelingSalesman edges =
  minimum . map finish . assocs . travelingSalesmanNoReturn $ edges
  where
    finish (k, d) = d + edges ! (k .&. 7)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . minimum
    . elems
    . travelingSalesmanNoReturn
    . findPaths
    . fromRight (error "parser failed")
    . flip evalState 0
    . runParserT parseInput "day24"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . travelingSalesman
    . findPaths
    . fromRight (error "parser failed")
    . flip evalState 0
    . runParserT parseInput "day24"
