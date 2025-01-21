module Day6
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (filter)
import           Data.Word8                 (_space)
import           Helpers.Parsers.ByteString (signedDoubles)

toPair :: [[Double]] -> [(Double, Double)]
toPair (a:b:_) = zip a b

-- The problem boils down to x(t - x) > r, that is x² - tx + r < 0,
-- that is any integer between the roots of x^2 - tx + r
quadraticSolution :: (Double, Double) -> Int
quadraticSolution (t, r) = maxX - minX + 1
  where
    minX = ceiling ((t - sqrt (t ^ 2 - 4 * r)) / 2)
    maxX = floor ((t + sqrt (t ^ 2 - 4 * r)) / 2)

part1 :: Bool -> ByteString -> String
part1 _ = show . product . map quadraticSolution . toPair . signedDoubles

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . quadraticSolution
    . head
    . toPair
    . signedDoubles
    . B.filter (/= _space)
