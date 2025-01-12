module Day1
  ( part1
  , part2
  ) where

import           Data.ByteString      (ByteString)
import           Data.ByteString.UTF8 (toString)
import           Data.List            (elemIndex)
import           Data.Maybe           (fromJust)

calc :: Char -> Int -> Int
calc ')'  = (+ (-1))
calc '('  = (+ 1)
calc '\n' = id

part1 :: Bool -> ByteString -> String
part1 _ = show . foldr calc 0 . toString

part2 :: Bool -> ByteString -> String
part2 _ = show . fromJust . elemIndex (-1) . scanl (flip calc) 0 . toString
