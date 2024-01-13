module Day15
  ( part1
  , part2
  ) where

import           Data.Graph.Inductive.Graph        (labNodes)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.BFS    (esp, level)
import           Data.HashSet                      as St (HashSet, empty,
                                                          insert, member,
                                                          singleton, toList)
import           Data.List                         as L (unfoldr)
import           Data.Sequence                     as Sq (Seq ((:|>)),
                                                          ViewL (EmptyL, (:<)),
                                                          singleton, viewl)
import           Helpers.Graph                     (Pos, assocsToDigraph, dirs,
                                                    origin)
import           Intcode                           (Intcode, initialise,
                                                    runIntcode, sendInput)

graphBuilder ::
     (Seq ((Pos, Int), Intcode), HashSet (Pos, Int))
  -> Maybe ( ((Pos, Int), [((Pos, Int), Int)])
           , (Seq ((Pos, Int), Intcode), HashSet (Pos, Int)))
graphBuilder (toSee, seen)
  | decons == EmptyL = Nothing
  | otherwise =
    Just ((curVal, map (\x -> (fst x, 1)) toConsider), (newToSee, newSeen))
  where
    decons = viewl toSee
    ((curVal@(curPos, _), curCode) :< rest) = decons
    toConsider =
      filter (\(x, _) -> snd x /= 0 && not (x `member` seen)) .
      map (\(a, (b, c)) -> ((a, head b), c)) .
      zipWith (\a b -> (curPos + a, runIntcode . sendInput b $ curCode)) dirs $
      [1, 2, 4, 3]
    newToSee = foldr (flip (:|>)) rest toConsider
    newSeen = foldr (insert . fst) seen toConsider

buildGraph :: Intcode -> Gr (Pos, Int) Int
buildGraph intcode =
  assocsToDigraph . unfoldr graphBuilder $
  (Sq.singleton ((origin, 1), intcode), St.singleton (origin, 1))

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
