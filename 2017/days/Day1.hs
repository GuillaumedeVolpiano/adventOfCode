module Day1
  ( part1
  , part2
  ) where

import           Data.Char   (digitToInt, isDigit)
import           Data.List   as L (length)
import           Data.Vector as V (Vector, fromList, length, (!))

count :: Int -> Vector Int -> Int
count step captcha = foldr compute 0 [0 .. V.length captcha - 1]
  where
    compute v acc =
      comp (captcha ! v) (captcha ! ((v + step) `mod` V.length captcha)) + acc

comp :: Int -> Int -> Int
comp a b
  | a == b = a
  | otherwise = 0

part1 :: Bool -> String -> String
part1 _ = show . count 1 . fromList . map digitToInt . filter isDigit

part2 :: Bool -> String -> String
part2 _ input =
  show
    . count (L.length input `div` 2)
    . fromList
    . map digitToInt
    . filter isDigit
    $ input
