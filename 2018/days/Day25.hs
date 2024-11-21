module Day25
  ( part1
  , part2
  ) where

import           Data.Either     (fromRight)
import           Data.List       as L (filter, foldr)
import           Data.Set        as S (Set, delete, filter, foldr, fromList,
                                       insert, intersection, map, null, union)
import           Helpers.Parsers (nums, parseInput)
import           Text.Megaparsec (parse)

type Point = [Int]

buildConstellations :: Set Point -> [Set Point]
buildConstellations points = loop constellations
  where
    constellations = fst . S.foldr close ([], points) $ points

close :: Point -> ([Set Point], Set Point) -> ([Set Point], Set Point)
close point (seen, points) = (closePoints : seen, points)
  where
    closePoints = S.filter (closeBy point) points

reduce :: [Set Point] -> [Set Point]
reduce [] = []
reduce (p:ps) =
  (L.foldr union p . L.filter (not . S.null . intersection p) $ ps)
    : (reduce . L.filter (S.null . intersection p) $ ps)

loop :: [Set Point] -> [Set Point]
loop points
  | reduce points == points = points
  | otherwise = loop . reduce $ points

closeBy :: Point -> Point -> Bool
closeBy a = (<= 3) . sum . fmap abs . zipWith (-) a

part1 :: Bool -> String -> String
part1 _ =
  show
    . length
    . buildConstellations
    . S.fromList
    . fromRight []
    . parse (parseInput nums) ""

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
