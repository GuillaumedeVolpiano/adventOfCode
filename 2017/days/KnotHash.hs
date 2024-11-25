module KnotHash
  ( hash
  , fullHash
  , initialCircle
  ) where

import           Data.Bits     (xor)
import           Data.Char     (ord)
import           Data.List     as L (foldl')
import           Data.Sequence as S (Seq, chunksOf, fromList, index, length,
                                     reverse, splitAt, (><))

initialCircle test
  | test = fromList [0 .. 4]
  | otherwise = fromList [0 .. 255]

extension = [17, 31, 73, 47, 23]

knot :: (Int, Int, Seq Int) -> Int -> (Int, Int, Seq Int)
knot (curpos, skip, circle) aLength =
  ( (curpos + skip + aLength) `mod` S.length circle
  , (skip + 1) `mod` S.length circle
  , untouched <> additional)
  where
    (toReverse, rest) = S.splitAt aLength circle
    int = rest <> S.reverse toReverse
    (additional, untouched) = S.splitAt skip int

hash :: (Int, Int, Seq Int) -> [Int] -> (Int, Int, Seq Int)
hash (pos, skip, circle) = foldl' knot (pos, skip, circle)

dense :: (Int, Int, Seq Int) -> [Int]
dense (curPos, _, circle) = concatMap (hexify . densify) . chunksOf 16 $ rewound
  where
    origin = S.length circle - curPos
    (before, after) = S.splitAt origin circle
    rewound = after <> before
    densify = foldr xor 0
    hexify x = [div x 16, mod x 16]

fullHash :: String -> [Int]
fullHash input =
  dense . (!! 64) . iterate (`hash` lengthList) $ (0, 0, initialCircle False)
  where
    lengthList = (++ extension) . map ord . filter (/= '\n') $ input
