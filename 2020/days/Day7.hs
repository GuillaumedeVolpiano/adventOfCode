module Day7
  ( part1
  , part2
  ) where

import           Control.Monad                  (void)
import           Data.Char                      (isDigit)
import           Data.Graph.Inductive.Graph     (Graph, Node, labNodes, lsuc)
import           Data.Graph.Inductive.Query.BFS (bfs)
import           Data.Maybe                     (Maybe (Just, Nothing),
                                                 catMaybes)
import           Helpers.Graph                  (assocsToGraph,
                                                 assocsToReverseGraph)
import           Helpers.Parsers                (Parser, parseByLine)
import           Text.Megaparsec                (manyTill, takeWhile1P, try,
                                                 (<|>))
import           Text.Megaparsec.Char           (char, digitChar, eol,
                                                 printChar, string)

parser :: Parser (String, [(String, Int)])
parser = do
  node <- manyTill printChar (string " bags contain ")
  edges <- manyTill edge eol
  return (node, catMaybes edges)
  where
    edge = try someBags <|> noBags
    consumeBag =
      try (string "s, ") <|> try (string ", ") <|> try (string "s.") <|>
      string "."
    someBags = do
      num <- takeWhile1P Nothing isDigit
      void $ char ' '
      node <- manyTill printChar (string " bag")
      void consumeBag
      return . Just $ (node, read num)
    noBags = do
      void . string $ "no other bags."
      return Nothing

findGoldBag :: (Graph gr) => gr String Int -> [Node]
findGoldBag invRules = bfs (goldBag invRules) invRules

goldBag :: (Graph gr) => gr String Int -> Node
goldBag = fst . head . filter ((== "shiny gold") . snd) . labNodes

foldRules :: (Graph gr) => gr String Int -> Node -> Int
foldRules rules =
  (1 +) . sum . map (\(a, b) -> b * foldRules rules a) . lsuc rules

emptyGoldBag :: (Graph gr) => gr String Int -> Int
emptyGoldBag rules = foldRules rules . goldBag $ rules

part1 :: Bool -> String -> String
part1 _ =
  show .
  (+ (-1)) . length . findGoldBag . assocsToReverseGraph . parseByLine parser

part2 :: Bool -> String -> String
part2 _ = show . (+ (-1)) . emptyGoldBag . assocsToGraph . parseByLine parser
