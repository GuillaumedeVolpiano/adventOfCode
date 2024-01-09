module Day4
  ( part1
  , part2
  ) where

import           Data.Char       (digitToInt)
import           Data.List.Split (splitOn)

range = [0 .. 9]

potentials :: [[Int]] -> [[Int]]
potentials [l1@[x1, x2, x3, x4, x5, x6], l2@[y1, y2, y3, y4, y5, y6]] =
  [ [a, b, c, d, e, f]
  | a <- range
  , b <- range
  , a <= b
  , c <- range
  , b <= c
  , d <- range
  , c <= d
  , e <- range
  , d <= e
  , f <- range
  , e <= f
  , mkNumber [a, b, c, d, e, f] >= minn && mkNumber [a, b, c, d, e, f] <= maxn
  , a == b || b == c || c == d || d == e || e == f
  ]
  where
    mkNumber = foldl (\a b -> b + 10 * a) 0
    minn = mkNumber l1
    maxn = mkNumber l2

part1 :: Bool -> String -> String
part1 _ = show . length . potentials . map (map digitToInt) . splitOn "-" . init

part2 :: Bool -> String -> String
part2 _ =
  show .
  length .
  filter
    (\[a, b, c, d, e, f] ->
       (a == b && b /= c) ||
       (a /= b && b == c && c /= d) ||
       (b /= c && c == d && d /= e) ||
       (c /= d && d == e && e /= f) || (d /= e && e == f)) .
  potentials . map (map digitToInt) . splitOn "-" . init
