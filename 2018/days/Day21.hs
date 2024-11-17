module Day21
  ( part1
  , part2
  ) where

import           Data.Bits ((.&.), (.|.))
import           Data.List (unfoldr)
import           Data.Set  (Set, empty, insert, member)

-- lines 0-4 check Bani
-- 5 R1 = 0
-- 6 R5 = R1 .|. 65536
-- 7 R1 = 8595037
-- 8 R3 = R5 .&. 255
-- 9 R1 =  R1 + R3
-- 10 R1 = R1 .&. 16777215
-- 11 R1 = R1 * 65899
-- 12 R1 = R1 .&. 16777215
-- 13-27 if R5 < 256 goto 28 else (R5 = R5 // 256; goto 8)
-- 28 if R0 = R1 then halt else goto 6
check :: Int -> Int
check = loop 8595037 . (.|. 2 ^ 16)

loop :: Int -> Int -> Int
loop r1 r5
  | r5 < 256 = newR1
  | otherwise = loop newR1 (div r5 256)
  where
    r3 = r5 `mod` 256
    newR1 = (((r1 + r3) `mod` 2 ^ 24) * 65899) `mod` 2 ^ 24

findAll :: (Int, Set Int) -> Maybe (Int, (Int, Set Int))
findAll (seed, seen)
  | r1 `member` seen = Nothing
  | otherwise = Just (r1, (r1, insert r1 seen))
  where
    r1 = check seed

part1 :: Bool -> String -> String
part1 _ _ = show . check $ 0

part2 :: Bool -> String -> String
part2 _ _ = show . last . unfoldr findAll $ (0, empty)
