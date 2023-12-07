module Day20
  ( part1
  , part2
  ) where

import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.CircularList  (CList, focus, fromList, insertR, removeR,
                                     rightElements, rotN, rotR, rotateTo, size,
                                     toList)
import           Data.Maybe         (fromJust)

decryptionKey = 811589153

index1 = 1000

index2 = 2000

index3 = 3000

parseInput :: [String] -> [(Int, Int)]
parseInput = zip [1 ..] . map read

move :: (Int, Int) -> CList (Int, Int) -> CList (Int, Int)
move pair@(_, number) clist =
  insertR pair . rotN modded . removeR . fromJust . rotateTo pair $ clist
  where
    modded = mod number (size clist - 1)

mix :: CList (Int, Int) -> [(Int, Int)] -> CList (Int, Int)
mix = foldl (flip move)

remix :: [(Int, Int)] -> CList (Int, Int) -> CList (Int, Int)
remix = flip mix

score :: (Int, Int) -> CList (Int, Int) -> Int
score zero clist = score1 + score2 + score3
  where
    zlist = fromJust . rotateTo zero $ clist
    score1 = snd . fromJust . focus . rotN index1 $ zlist
    score2 = snd . fromJust . focus . rotN index2 $ zlist
    score3 = snd . fromJust . focus . rotN index3 $ zlist

decrypt :: (Int, Int) -> (Int, Int)
decrypt (a, b) = (a, decryptionKey * b)

part1 :: Bool -> String -> String
part1 _ input = show . score zero . mix file $ indices
  where
    indices = parseInput . lines $ input
    file = fromList indices
    zero = head . filter (\x -> snd x == 0) $ indices

part2 :: Bool -> String -> String
part2 _ input =
  show . score zero . last . take 11 . iterate (remix decryptedIndices) $
  decryptedFile
  where
    indices = parseInput . lines $ input
    file = fromList indices
    zero = head . filter (\x -> snd x == 0) $ indices
    decryptedFile = fmap decrypt file
    decryptedIndices = map decrypt indices
