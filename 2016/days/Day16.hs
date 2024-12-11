module Day16
  ( part1
  , part2
  ) where

import           Data.Bits            (bit, complement, shiftL, shiftR)
import           Data.Char            (intToDigit)
import           Data.Text            (Text, unpack)
import           Helpers.Parsers.Text

-- to be continued. The final bits are going to be of the form a · b · a · b · a
-- ·b…  In the end, there will be k ab pairs, and (2k - 1) separators.The total
-- length of the filler will be 2k (length a + 1) - 1, that is 2k length a - 1,
-- which will then need to be trimmed to the actual length of the filler. The
-- size of the chunks will be the highest power of 2 that divide the disk size.
-- The parity of the chunk will be determined by the parity of the separators +
-- that of the potential trailing or starting parts of an ab pair.

bitSize :: Int -> Int
bitSize = (1 +) . floor . logBase 2 . fromIntegral . (1 +)

bitReverse :: Int -> Int -> Int
bitReverse a b
  | a == 0 = b
  | otherwise = bitReverse (shiftR a 1) . (+ shiftL b 1) $ a `mod` 2

part1 :: Bool -> Text -> String
part1 _ = show . bitSize . read . init . unpack

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
