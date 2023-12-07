module Day3 where

import           Data.Char       (ord)
import           Data.List       as L (map)
import           Data.List.Split (chunksOf)
import           Data.Set        as S (elemAt, fromList, intersection, map)

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  L.map
    ((\(x, y) ->
        elemAt 0 . S.map (\x -> mod (ord x - 96) 58) . intersection (fromList x) $
        fromList y) .
     (\x -> splitAt (div (length x) 2) x)) .
  lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  L.map
    ((\x -> mod (ord x - 96) 58) .
     elemAt 0 . foldl1 intersection . L.map fromList) .
  chunksOf 3 . lines
