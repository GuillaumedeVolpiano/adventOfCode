module Day10
  ( part1
  , part2
  ) where

import           Data.List (group, groupBy, nub, sort, sortBy, tails)
import           Linear.V2 (V2 (..))

type Pos = V2 Int

type AstMap = [Pos]

type Line = (Maybe Slope, Offset)

type Slope = Rational

type Offset = Rational

line :: Pos -> Pos -> Line
line from@(V2 xf yf) to@(V2 xt yt)
  | xf == xt = (Nothing, fromIntegral xf)
  | otherwise = (Just slope, offset)
  where
    slope = fromIntegral (yf - yt) / fromIntegral (xf - xt) :: Rational
    offset = fromIntegral yf - slope * fromIntegral xf

between :: Pos -> Pos -> Pos -> Bool
between from@(V2 xf yf) to@(V2 xt yt) pos@(V2 a b) =
  pos /= from && pos /= to && xm <= a && a <= xM && ym <= b && b <= yM
  where
    xm = min xf xt
    xM = max xf xt
    ym = min yf yt
    yM = max yf yt

selectVisible :: [(Pos, Pos)] -> [Pos]
selectVisible aList =
  unpair . filter (\(a, b) -> not . any (between a b) $ nubbed) $ aList
  where
    nubbed = nub . unpair $ aList
    unpair = concatMap (\(a, b) -> [a, b])

findBest :: AstMap -> Int
findBest astMap =
  maximum .
  map length .
  group .
  sort .
  concatMap (selectVisible . map (\(a, b, _) -> (a, b))) .
  groupBy (\(_, _, a) (_, _, b) -> a == b) .
  sortBy (\(_, _, a) (_, _, b) -> compare a b) .
  concat .
  zipWith
    (\a b -> map (\x -> (a, x, line a x)) . filter (/= a) $ b)
    (init astMap) $
  tails astMap

buildMap :: String -> AstMap
buildMap =
  concat .
  zipWith (\a -> map (`V2` a)) [0 ..] .
  map (map fst . filter ((== '#') . snd) . zip [0 ..]) . lines

part1 :: Bool -> String -> String
part1 _ = show . findBest . buildMap

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
