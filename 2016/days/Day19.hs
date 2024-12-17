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

type Elf = Int

type Index = Int

type Presents = Int

makeElves :: Int -> Seq Elf
makeElves total = fromList [1 .. total]

distribute :: Int -> Int
distribute elves = (clearBit elves msb `shiftL` 1) + 1
  where
    msb = finiteBitSize elves - countLeadingZeros elves - 1

distributeOpposite :: Seq Elf -> Int
distributeOpposite elves
  | Sq.length elves == 1 = e
  | otherwise = distributeOpposite elves''
  where
    (e :<| elves') = elves
    mid = div (Sq.length elves') 2
    (before, loser, after)
      | even (Sq.length elves') = (b, l, a)
      | odd (Sq.length elves') = (b', l', a')
      where
        (b :|> l, a) = Sq.splitAt mid elves'
        (b', l' :<| a') = Sq.splitAt mid elves'
    elves'' = before >< (after :|> e)

part1 :: Bool -> Text -> String
part1 _ = show . distribute . head . head . signedInts

part2 :: Bool -> Text -> String
part2 _ = show . distributeOpposite . makeElves . head . head . signedInts
