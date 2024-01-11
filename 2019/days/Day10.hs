{-# LANGUAGE TupleSections #-}

module Day10
  ( part1
  , part2
  ) where

import           Data.List (group, sort, tails)
import           Linear.V2 (V2 (..))

type Pos = V2 Int

type AstMap = [Pos]

isVisible :: AstMap -> Pos -> Pos -> Bool
isVisible astmap from@(V2 xf yf) to@(V2 xt yt) =
  from /= to && (not . any curBetween $ astmap)
  where
    curLine = line from to
    curBetween = between curLine from to

line :: Pos -> Pos -> Pos -> Bool
line from@(V2 xf yf) to@(V2 xt yt) (V2 a b)
  | xf == xt = a == xf
  | otherwise = slope * fromIntegral a + offset == fromIntegral b
  where
    slope = fromIntegral (yf - yt) / fromIntegral (xf - xt) :: Rational
    offset = fromIntegral yf - slope * fromIntegral xf

between :: (Pos -> Bool) -> Pos -> Pos -> Pos -> Bool
between curLine from@(V2 xf yf) to@(V2 xt yt) pos@(V2 a b) =
  pos /= from &&
  pos /= to && curLine pos && xm <= a && a <= xM && ym <= b && b <= yM
  where
    xm = min xf xt
    xM = max xf xt
    ym = min yf yt
    yM = max yf yt

findBest :: AstMap -> Int
findBest astMap =
  maximum .
  map length .
  group .
  sort .
  concatMap (\(a, b) -> [a, b]) .
  filter (uncurry (isVisible astMap)) .
  concat . zipWith (\a b -> map (a, ) b) (init astMap) $
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
