module Day8
  ( part1
  , part2
  ) where

import           Data.Map        as M (Map, fromList, keys, (!))
import           Data.Maybe      (fromJust)
import           Data.Text       as T (Text, last, pack)
import           Search          (bfsDist)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

type Tree = Map Pos (Pos, Pos)

type Prune = Map Pos [Pos]

type Instructions = [Char]

type Pos = Text

type Step = Int

test2 =
  "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"

pruneTree :: (Instructions, Tree) -> (Step, Prune)
pruneTree (instructions, tree) = (length instructions, prunedTree)
  where
    prunedTree = fromList . map (\a -> (a, follow instructions a)) $ keys tree
    follow [] p     = [p]
    follow (x:xs) p = follow xs (translateInst x . (!) tree $ p)

parseLine :: [String] -> (Step, Prune)
parseLine input = pruneTree (head input, tree)
  where
    rawTree =
      map (\a -> getAllTextMatches (a =~ "[0-9A-Z]+") :: [String]) . drop 2 $
      input
    tree = M.fromList . map (\(a:b:c:_) -> (pack a, (pack b, pack c))) $ rawTree

translateInst :: Char -> ((a, a) -> a)
translateInst 'L' = fst
translateInst 'R' = snd
translateInst i   = error ("can't understand instruction " ++ show i)

part1 :: Bool -> String -> String
part1 _ input = show . (*) step $ dist
  where
    (step, prune) = parseLine . lines $ input
    dist = bfsDist (pack "AAA") (prune !) (== pack "ZZZ")

part2 :: Bool -> String -> String
part2 test input = show . (*) step . foldl1 lcm $ dists
  where
    toParse
      | test = test2
      | otherwise = input
    (step, prune) = parseLine . lines $ toParse
    dists =
      map (\a -> bfsDist a (prune !) (\t -> T.last t == 'Z')) .
      filter (\t -> T.last t == 'A') . keys $
      prune
