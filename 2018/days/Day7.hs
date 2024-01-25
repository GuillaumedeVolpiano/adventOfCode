module Day7
  ( part1
  , part2
  ) where

import           Control.Monad              (void)
import           Data.Char                  (ord)
import           Data.Either                (fromRight)
import           Data.Graph.Inductive.Graph (delNode, indeg, labNodes, order)
import           Data.List                  (delete, groupBy, minimumBy, nub,
                                             sortBy, (\\))
import           Helpers.Graph              (Gr, assocsToGraph)
import           Helpers.Parsers            (Parser)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (char, eol, string, upperChar)

parser :: Parser [(Char, [(Char, Int)])]
parser =
  map (foldr (\(a, b) (_, c) -> (a, (b, 1) : c)) (' ', [])) .
  groupBy (\(a, _) (b, _) -> a == b) . sortBy (\(a, _) (b, _) -> compare a b) <$>
  manyTill parseLine eof

parseLine :: Parser (Char, Char)
parseLine = do
  void . string $ "Step "
  from <- upperChar
  void . string $ " must be finished before step "
  to <- upperChar
  void . string $ " can begin."
  void eol
  return (from, to)

oneStep :: Gr Char Int -> String
oneStep instructions
  | order instructions == 0 = ""
  | otherwise = curLab : oneStep (delNode curNode instructions)
  where
    (curNode, curLab) =
      minimumBy (\(_, a) (_, b) -> compare a b) .
      filter ((== 0) . indeg instructions . fst) . labNodes $
      instructions

multiStep :: Int -> Int -> [(Int, Int)] -> Gr Char Int -> Int
multiStep delay numWorkers workers instructions
  | order instructions == 0 && null workers = 0
  | length workers == numWorkers || null roots =
    mw + multiStep delay numWorkers updatedWorkers (delNode mnode instructions)
  | otherwise =
    multiStep delay numWorkers ((curNode, curVal) : workers) instructions
  where
    (mnode, mw) = minimumBy (\(_, a) (_, b) -> compare a b) workers
    updatedWorkers = map (\(n, v) -> (n, v - mw)) . delete (mnode, mw) $ workers
    curVal = ord curLab - ord 'A' + 1 + delay
    (curNode, curLab) = minimumBy (\(_, a) (_, b) -> compare a b) roots
    roots =
      filter
        (\(n, _) -> indeg instructions n == 0 && n `notElem` map fst workers) .
      labNodes $
      instructions

part1 :: Bool -> String -> String
part1 _ = show . oneStep . assocsToGraph . fromRight [] . parse parser ""

part2 :: Bool -> String -> String
part2 test =
  show .
  multiStep delay numWorkers [] . assocsToGraph . fromRight [] . parse parser ""
  where
    delay
      | test = 0
      | otherwise = 60
    numWorkers
      | test = 2
      | otherwise = 5
