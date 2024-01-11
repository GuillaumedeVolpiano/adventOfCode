module Day6
  ( part1
  , part2
  ) where

import           Control.Monad                     (void)
import           Data.Either                       (Either (Right), isLeft)
import           Data.Graph.Inductive.Graph        (labNodes)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.BFS    (esp, level)
import           Helpers.Graph                     (assocsToDigraph)
import           Helpers.Parsers                   (Parser)
import           Text.Megaparsec                   (many, optional, parse,
                                                    takeWhile1P)
import           Text.Megaparsec.Char              (char, eol, letterChar)

type Orbits = Gr String Int

parseOrbits :: Parser Orbits
parseOrbits = assocsToDigraph <$> many parseLine

parseLine :: Parser (String, [(String, Int)])
parseLine = do
  n1 <- takeWhile1P Nothing (/= ')')
  void . char $ ')'
  n2 <- takeWhile1P Nothing (/= '\n')
  void . optional $ eol
  return (n1, [(n2, 1)])

buildGraph :: String -> Orbits
buildGraph input
  | isLeft parsed = error ("couldn't parse " ++ show parsed)
  | otherwise = jParsed
  where
    parsed = parse parseOrbits "" input
    (Right jParsed) = parsed

findOrbits :: Orbits -> Int
findOrbits orbits = sum . map snd . level com $ orbits
  where
    com = fst . head . filter (\x -> snd x == "COM") . labNodes $ orbits

-- we need to remove 3 because we are not counting the first and the last jumps,
-- and we are counting edges and not nodes.
shortPathToSanta :: Orbits -> Int
shortPathToSanta orbits = (-3 +) . length . esp you santa $ orbits
  where
    you = fst . head . filter (\x -> snd x == "YOU") $ nodes
    santa = fst . head . filter (\x -> snd x == "SAN") $ nodes
    nodes = labNodes orbits

part1 :: Bool -> String -> String
part1 _ = show . findOrbits . buildGraph

part2 :: Bool -> String -> String
part2 _ = show . shortPathToSanta . buildGraph
