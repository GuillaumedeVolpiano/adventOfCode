module Day8
  ( part1
  , part2
  ) where

import           Data.Char       (digitToInt)
import           Data.List       (group, minimumBy, sort)
import           Data.List.Split (chunksOf)

render :: [Int] -> [Int] -> [Int]
render =
  zipWith
    (\x y ->
       if x == 2
         then y
         else x)

stringify :: [Int] -> String
stringify = unlines . chunksOf 25 . map charify
  where
    charify 1 = '#'
    charify 0 = ' '

part1 :: Bool -> String -> String
part1 _ =
  show .
  product .
  map length .
  tail .
  minimumBy (\a b -> compare (length . head $ a) (length . head $ b)) .
  map (group . sort) . chunksOf 150 . map digitToInt . init

part2 :: Bool -> String -> String
part2 _ =
  stringify . foldl render (repeat 2) . chunksOf 150 . map digitToInt . init
