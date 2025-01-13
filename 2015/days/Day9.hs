{-# LANGUAGE TemplateHaskell #-}

module Day9
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Char                 (isAlpha)
import           Data.List                 (sortBy, subsequences)
import           Data.Map                  (Map, elems, filterWithKey, insert,
                                            keys, (!))
import           Data.Ord                  (Down (..), comparing)
import           Data.Set                  (Set, delete, findMin, fromList,
                                            size, toList)
import qualified Data.Set                  as S (foldr)
import           Data.Void                 (Void)
import           FlatParse.Basic           (anyAsciiDecimalInt, char, eof, many,
                                            runParser, satisfy, string, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Routes = Map (String, String) Int

type BestRoutes = Map (Set String, String) Int

type Parser = F.Parser Routes

parseInput :: Parser
parseInput = parseLine <|> (eof >> return mempty)

parseLine :: Parser
parseLine = do
  from <- many . satisfy $ isAlpha
  $(string " to ")
  to <- many . satisfy $ isAlpha
  $(string " = ")
  dist <- anyAsciiDecimalInt
  $(char '\n')
  insert (from, to) dist . insert (to, from) dist <$> parseInput

travelingSalesman :: ([Int] -> Int) -> Routes -> Int
travelingSalesman selection edges =
  selection . elems . filterWithKey (\(a, _) _ -> size a == 8) $ findSubsets
  where
    intermediateRoutes =
      map fromList
        . sortBy (comparing (Down . length))
        . tail
        . subsequences
        . toList
        . fromList
        . map fst
        . keys
        $ edges
    findSubsets = foldr mapSubset mempty intermediateRoutes
    mapSubset set subsets
      | size set == 1 = insert (set, findMin set) 0 subsets
      | otherwise = S.foldr (subsetise set) subsets set
    subsetise set k subsets =
      insert (set, k) (bestSub subsets k . delete k $ set) subsets
    bestSub subsets k set =
      selection [subsets ! (set, m) + edges ! (m, k) | m <- toList set]

part1 :: Bool -> ByteString -> String
part1 _ = show . travelingSalesman minimum . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . travelingSalesman maximum . extract . runParser parseInput
