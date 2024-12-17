module Day19
  ( part1
  , part2
  ) where

import           Data.Bits            (clearBit, countLeadingZeros,
                                       finiteBitSize, shiftL)
import           Data.Sequence        as Sq (Seq ((:<|), (:|>)), fromList,
                                             length, splitAt, (><))
import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

import           Debug.Trace

type Elf = Int

type Index = Int

type Presents = Int

makeElves :: Int -> Seq Elf
makeElves total = fromList [1 .. total]

-- some bitwise voodoo
distribute :: Int -> Int
distribute elves = (clearBit elves msb `shiftL` 1) + 1
  where
    msb = finiteBitSize elves - countLeadingZeros elves - 1

midSplit :: Seq Elf -> (Seq Elf, Seq Elf)
midSplit elves = Sq.splitAt (Sq.length elves `div` 2) elves

distributeOpposite :: Seq Elf -> Seq Elf -> Int
distributeOpposite before after
  | Sq.length before == 0 && Sq.length after == 1 = loser
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
  show
    . uncurry distributeOpposite
    . midSplit
    . makeElves
    . head
    . head
    . signedInts
