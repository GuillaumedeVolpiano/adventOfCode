module Day15
  ( part1
  , part2
  ) where

import           Data.Graph.Inductive.Graph     (labNodes)
import           Data.Graph.Inductive.Query.BFS (esp, level)
import           Data.HashSet                   as St (HashSet, empty, insert,
                                                       member, singleton,
                                                       toList)
import           Data.List                      as L (unfoldr)
import           Data.Sequence                  as Sq (Seq ((:|>)),
                                                       ViewL (EmptyL, (:<)),
                                                       singleton, viewl)
import           Helpers.Graph                  (Gr, Pos, assocsToDigraph, dirs,
                                                 origin, unfoldAssocs)
import           Intcode                        (Intcode, initialise,
                                                 runIntcode, sendInput)

buildGraph :: Intcode -> Gr (Pos, Int) Int
buildGraph intcode =
  assocsToDigraph . unfoldr (unfoldAssocs buildCons fst) $
  (Sq.singleton ((origin, 1), intcode), St.singleton (origin, 1))
  where
    buildCons set pos =
      filter (\((x, _), _) -> snd x /= 0 && not (x `member` set)) .
      map (\(a, (b, c)) -> (((a, head b), c), 1)) .
      zipWith
        (\a b -> (fst (fst pos) + a, runIntcode . sendInput b . snd $ pos))
        dirs $
      [1, 2, 4, 3]

oxygenTank :: Gr (Pos, Int) Int -> Int
oxygenTank = fst . head . filter ((== 2) . snd . snd) . labNodes

findOxygenTank :: Intcode -> Int
findOxygenTank intcode = (-1 +) . length . esp 0 oxTank $ area
  where
    area = buildGraph intcode
    oxTank = oxygenTank area

fillArea :: Intcode -> Int
fillArea intcode = maximum . map snd . level oxTank $ area
  where
    area = buildGraph intcode
    oxTank = oxygenTank area

part1 :: Bool -> String -> String
part1 _ = show . findOxygenTank . initialise

part2 :: Bool -> String -> String
part2 _ = show . fillArea . initialise
