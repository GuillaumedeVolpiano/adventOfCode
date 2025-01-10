module Day19
  ( part1
  , part2
  ) where

import           Data.Bits            (clearBit, countLeadingZeros,
                                       finiteBitSize, shiftL)
import           Data.Sequence        as Sq (Seq ((:<|), (:|>)), fromList,
                                             length, splitAt, (><), null)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

import           Debug.Trace

type Elf = Int

type Index = Int

type Presents = Int

makeElves :: Int -> Seq Elf
makeElves total = fromList [1 .. total]

-- some bitwise voodoo (O(1) complexity) https://en.wikipedia.org/wiki/Josephus_problem#Bitwise
distribute :: Int -> Int
distribute elves = (clearBit elves msb `shiftL` 1) + 1
  where
    msb = finiteBitSize elves - countLeadingZeros elves - 1

-- using two sequences for O(1) access to the middle element. If the overall
-- number of elves is odd, the after sequence is going to be the longest. The
-- overall complexity is O(n)
midSplit :: Int -> (Seq Elf, Seq Elf)
midSplit elves = (fromList [1 .. mid], fromList [mid + 1 .. elves])
  where
    mid = elves `div` 2

distributeOpposite :: Seq Elf -> Seq Elf -> Int
distributeOpposite before after
  | Sq.null before = loser
  | otherwise = distributeOpposite before'' after''
  where
    (e :<| before') = before
    (loser :<| after') = after
    (before'', after'')
      | Sq.length after' == Sq.length before' = (before', after' :|> e)
      | otherwise = (before' :|> a, fter' :|> e)
      where
        (a :<| fter') = after'

part1 :: Bool -> Text -> String
part1 _ = show . distribute . head . head . signedInts

part2 :: Bool -> Text -> String
part2 _ =
  show . uncurry distributeOpposite . midSplit . head . head . signedInts
